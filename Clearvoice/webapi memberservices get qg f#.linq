<Query Kind="FSharpProgram">
</Query>

let dc = new TypedDataContext()

// http://localhost:18217/Help/Api/GET-api-Api.V1.Controllers.QuotaGroups-GetQuotaGroup_QuotaGroupGuid
// http://localhost:18217/api/v1/quotagroups/getquotagroup?quotagroupguid=

let useSprocCallToTestAuth = false
let host = "http://localhost:18217/"

let orgs = 
	
	dc.Org_api_logins
	|> Seq.map (fun o-> o.Org_id, o.Org.Org_name)
	|> Seq.distinct 
	
orgs.Dump("orgApiLogins")
	
let targetOrg =
	let orgMap = orgs |> Seq.map (fun (id,name) -> id)
	Util.ReadLine<int>("OrgId?",0, orgMap)
let orgApiLogin = dc.Org_api_logins.First(fun oa -> oa.Org_id=targetOrg)

let controller="QuotaGroups"; // "SurveyInventory";
let apiMethod ="GetQuotaGroup";// "GetSurveysForExternalMember";

let uri = sprintf "%sapi/%s/%s?QuotaGroupGuid=%s" host controller apiMethod "f1e55caf-7102-49c7-806e-f24637dcb6e2"
uri.Dump("url to call")
let webRequest = 
	let wc = System.Net.WebRequest.Create(uri)
	let authInfo = Convert.ToBase64String <| Encoding.Default.GetBytes( orgApiLogin.Username+":"+orgApiLogin.Password)
	wc.Headers.Add("Authorization: Basic "+authInfo)
	wc.Timeout <- 60000
	//wc.Accept <- "application/json"
	wc.ContentType <- "application/formurlencoded"
	wc
	
//type CreateProjectResponseF = { OrgGuid:System.Guid; ProjectGuid:System.Guid; ExternalSurveyId:int option}	

let comparisonProjectId = 66620
let apiResponse = 
	try
		use response = webRequest.GetResponse()
		response.Dump("response")
		use rStream = response.GetResponseStream()
		use r = new StreamReader(rStream,true)
		let rawResults= r.ReadToEnd()
		rawResults.Dump("results")
	with 
	| :? WebException as ex -> 
		use rStream = ex.Response.GetResponseStream()
		use r = new StreamReader(rStream,true)
		r.ReadToEnd().Dump("response ex")
		ex.Dump()
		failwith "response exception"
		
