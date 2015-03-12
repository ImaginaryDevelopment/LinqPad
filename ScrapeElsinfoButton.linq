<Query Kind="FSharpProgram">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>HtmlAgilityPack</Namespace>
  <Namespace>System.Net.Http</Namespace>
</Query>

type System.String with
	static member trim (s:string) = s.Trim()
	static member after (delimiter:string) (s:string) = 
		s.Substring <| s.IndexOf delimiter + delimiter.Length |> String.trim

type ICD9 =
	| Hypertension
	| Overweight
	| Obesity


let icd9 x =
	match x with
	| Hypertension -> 401.9
	| Overweight -> 278.02
	| Obesity -> 278.00

//type HL7Request = {
//	``mainSearchCriteria.v.dn``:string
//	``mainSearchCriteria.v.c``:ICD9 option
//	``mainSearchCriteria.v.cs``:string option
//	``age.v.v``:string option // age
//	``age.v.u``:string option // "a"
//	``ageGroup.v.c``:string option
//	``ageGroup.v.dn``:string option
//	``patientPerson.administrativeGenderCode.c``:string option
//	``patientPerson.administrativeGenderCode.dn``:string option
//	``informationRecipient.languageCode.c``:string option
//	``observation.v.c``:string option
//	``observation.v.cs``:string option
//	}

//let ``mainSearchCriteria.v.dn`` = "mainSearchCriteria.v.dn"
let request identifier searchCriteria = 
	let baseAddress = sprintf "http://www.elsinfobutton.com/info/%i" identifier
	let baseAddress = Uri(baseAddress)
	use client = new HttpClient(BaseAddress=baseAddress)
	client.GetStringAsync("?mainSearchCriteria.v.dn=Obesity").Result
let response = 
	let identifier = Int16.Parse <| Util.GetPassword "elsIdentifier"
	identifier.Dump("identifier");
	request identifier "Obesity"
let doc,resultsContainer = 
	let doc = new HtmlAgilityPack.HtmlDocument()
	doc.LoadHtml(response)
	doc,doc.GetElementbyId "resultsContainer"
type EInfoLink = {Title:string; Link:string}
type EResult = { Category:string; ResultSummary:string; EInfo:EInfoLink list}
let results =
	let mapLinkNode (node:HtmlNode) = {Title=node.InnerText;Link=node.Attributes.["href"].Value}
	let mapLinkNodes (nodes:HtmlNode seq) = 
		nodes
		|> Seq.map mapLinkNode 
		|> List.ofSeq
	let mapEResult index (categoryNode:HtmlNode) = 
		let xpath = sprintf "../ol[%i]//a" <| index + 1
		categoryNode.ChildNodes.[0].InnerText |> String.trim,
		categoryNode.ChildNodes.[1].InnerText |> String.trim |> String.after "Results",
		categoryNode.SelectNodes xpath |> mapLinkNodes

	resultsContainer.SelectNodes "//h2"
	|> Seq.mapi mapEResult
	|> Seq.map (fun (text, resultDisplay,links) -> 
		{
			Category=text
			ResultSummary=resultDisplay
			EInfo= links
		})
//	//resultsContainer.Descendants().Where(fun d-> d.Name = "h2")
results.Dump("results")
//resultsContainer.Dump("resultsList")
//doc.Dump("doc")
//response.Dump("response")


//let postReq identifier labs problems allergies meds differential finalDiagnosis vitalSigns = 
//	let allParams = stringjoin("&",[|labs;problems;allergies;meds;differential; finalDiagnosis; vitalSigns|])
//	sprintf "http://www.elsinfobutton.com/info/%i?"identifier  + allParams

	
//type AgeGroup = 
//	| Adolescent
//	| Adult	
	
	
//type VCs =
//	| ICD9
//	| ICD10
//	| Loinc
//	| SnoMed
//	| Cpt