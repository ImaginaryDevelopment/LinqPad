<Query Kind="FSharpProgram">
  <Connection>
  </Connection>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Configuration.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Framework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.ComponentModel.DataAnnotations.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.Caching.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.ApplicationServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.Services.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Utilities.v4.0.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Security.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.Protocols.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.EnterpriseServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Design.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Tasks.v4.0.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.ServiceProcess.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.RegularExpressions.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>System.Net</Namespace>
  <Namespace>System.Web</Namespace>
</Query>

let dc = new TypedDataContext()

let BuildPost (name:string, surveyurl:string, surveyLength:int, externalSurveyId:int option, partnerSalesperson, requestedCpi:decimal option):byte[] =
	// http://stackoverflow.com/questions/14702902/post-form-data-using-httpwebrequest
	let sb = new StringBuilder()
	let buildPair name value = sprintf "%s=%s" name value
	let encode (v:string) = HttpUtility.UrlEncode(v)
	let pairs = 
		seq {
			yield buildPair "Name" <| encode name
			yield buildPair "SurveyUrl" <| encode surveyurl
			yield buildPair "SurveyLength" <| surveyLength.ToString()
			if externalSurveyId.IsSome then 
				yield buildPair "ExternalSurveyId" <| externalSurveyId.Value.ToString()
			if String.IsNullOrEmpty(partnerSalesperson)=false then
				yield buildPair "PartnerSalesperson" <| encode partnerSalesperson
		} |> Seq.reduce (fun x y -> sprintf "%s&%s" x y)
	sb.Append(pairs) |> ignore
	
	let encoding = new ASCIIEncoding()
	encoding.GetBytes(sb.ToString())
	
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

let controller="Projects"; // "SurveyInventory";
let apiMethod ="CreateProject";// "GetSurveysForExternalMember";

if useSprocCallToTestAuth then 
	let shouldLoginSucceed = dc.OrgApiLogin_Authenticate(orgApiLogin.Username,orgApiLogin.Password,"::1",controller,apiMethod)
	shouldLoginSucceed.Dump("authenticate")
	
	if shouldLoginSucceed.Tables.Count<1 || shouldLoginSucceed.Tables.[0].Rows.Count<1 then  failwith "login will probably fail"
	
let uri = host+"api/"+controller+"/"+apiMethod
	
let wc = 
	let wc = System.Net.WebRequest.Create(uri)
	let authInfo = Convert.ToBase64String <| Encoding.Default.GetBytes( orgApiLogin.Username+":"+orgApiLogin.Password)
	wc.Headers.Add("Authorization: Basic "+authInfo)
	
	wc.Timeout <- 60000
	wc.Method <- "POST";
	wc.ContentType <- "application/x-www-form-urlencoded"
	
	let postData = BuildPost("Brandon ApiCreateProject"+DateTime.Now.ToString("yyyyMMddhh"),"helloSurveyUrl",5,None,null,None)
	wc.ContentLength <- postData.LongLength
	use postStream = wc.GetRequestStream()
	postStream.Write(postData,0,postData.Length);
	postStream.Flush();
	postStream.Close();
	wc
type CreateProjectResponseF = { OrgGuid:System.Guid; ProjectGuid:System.Guid; ExternalSurveyId:int option}	

let comparisonProjectId = 66620
let projectResponse = 
	try
		use response = wc.GetResponse()
		use rStream = response.GetResponseStream()
		use r = new StreamReader(rStream,true)
		let rawResults=
			let toDump = r.ReadToEnd()
			toDump.Dump()
			toDump
		Newtonsoft.Json.JsonConvert.DeserializeObject<CreateProjectResponseF>(rawResults)
	with 
	| :? WebException as ex -> 
		use rStream = ex.Response.GetResponseStream()
		use r = new StreamReader(rStream,true)
		r.ReadToEnd().Dump("response ex")
		ex.Dump()
		failwith "response exception"
		
projectResponse.Dump("response")
[ 
	dc.Projects.First(fun p->p.Project_id = comparisonProjectId)
	dc.Projects.First(fun p->p.Project_guid = projectResponse.ProjectGuid)
].Dump("vs")
	
