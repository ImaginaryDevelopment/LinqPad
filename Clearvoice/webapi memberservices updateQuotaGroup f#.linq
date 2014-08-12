<Query Kind="FSharpProgram">
  <Connection>
  </Connection>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>System.Net</Namespace>
  <Namespace>System.Web</Namespace>
</Query>

let dc = new TypedDataContext()

dc.Settings.Context.DeferredLoadingEnabled <- false
dc.Settings.Context.ObjectTrackingEnabled <- false
type PartialUpdate<'a> =
	| Ignore
	| Clear
	| Update of 'a

let orgs = 
	let orgApiLogins = query {
		for oal in dc.Org_api_logins do
		join org in dc.Orgs on
			(oal.Org_id = org.Org_id)
		select (oal.Org_id, org.Org_name)
	}
	orgApiLogins
	|> Seq.distinct 
	
// http://localhost:18217/Help/Api/POST-api-Api.V1.Controllers.Projects-CreateProject
type PostContent = 
	| FormEncoded
	| Json
	| Xml
	
type UpdateQuotaGroupRequestF = {QuotaGroupGuid:Guid; QuotaGroupName:PartialUpdate<String>}

let BuildPost postType (request:UpdateQuotaGroupRequestF):string*byte[]*string =
	// http://stackoverflow.com/questions/14702902/post-form-data-using-httpwebrequest
	let formEncode () = 
		let sb = StringBuilder()
		let buildPair name value = sprintf "%s=%s" name value
		let encode (v:string) = HttpUtility.UrlEncode(v)
		let pairs = 
			seq {
				yield buildPair "QuotaGroupGuid" <| encode (request.QuotaGroupGuid.ToString())
				match request.QuotaGroupName with
					| Ignore -> ()
					| Clear -> yield buildPair "QuotaGroupName" <| Unchecked.defaultof<string>
					| Update(x) -> yield buildPair "QuotaGroupName" <| encode x
			} |> Seq.reduce (fun x y -> sprintf "%s&%s" x y)
		sb.Append(pairs) |> ignore
		sb.ToString(),"application/x-www-form-urlencoded"
	let jsonEncode() = 
		Newtonsoft.Json.JsonConvert.SerializeObject <| request,
		"application/json"
	let text,contentType = match postType with
							| Json -> jsonEncode()
							| FormEncoded -> formEncode()
	let encoding = new ASCIIEncoding()
	text,encoding.GetBytes(text),contentType

let getQg,raw,postData,contentType = 
	let qgGuid = System.Guid("f1e55caf-7102-49c7-806e-f24637dcb6e2")
	let getQg()= 	
		dc.Quota_groups.First(fun qg->qg.Project_quota_guid = qgGuid)	
	let r,p,c = BuildPost Json {UpdateQuotaGroupRequestF.QuotaGroupGuid=qgGuid;QuotaGroupName=Ignore}
	getQg, r, p, c

let useSprocCallToTestAuth = false
let host = "http://localhost:18217/"

	
orgs.Dump("orgApiLogins") // display to user so that the readLine input is easier to do
	
let targetOrg =
	let orgMap = orgs |> Seq.map (fun (id,name) -> id)
	Util.ReadLine<int>("OrgId?",0, orgMap)
let orgApiLogin = dc.Org_api_logins.First(fun oa -> oa.Org_id=targetOrg)

let controller="QuotaGroups"; // "SurveyInventory";
let apiMethod ="UpdateQuotaGroup";// "GetSurveysForExternalMember";

if useSprocCallToTestAuth then 
	let shouldLoginSucceed = dc.OrgApiLogin_Authenticate(orgApiLogin.Username,orgApiLogin.Password,"::1",controller,apiMethod)
	shouldLoginSucceed.Dump("authenticate")
	
	if shouldLoginSucceed.Tables.Count<1 || shouldLoginSucceed.Tables.[0].Rows.Count<1 then  failwith "login will probably fail"
	
let uri = host+"api/"+controller+"/"+apiMethod


let before = getQg()

type UpdateQuotaGroupResponseF = { QuotaGroupGuid:System.Guid;}	


let updateQg (postData:byte[]) contentType = 
	let wc = 
		let wc = System.Net.WebRequest.Create(uri)
		let authInfo = Convert.ToBase64String <| Encoding.Default.GetBytes( orgApiLogin.Username+":"+orgApiLogin.Password)
		wc.Headers.Add("Authorization: Basic "+authInfo)
	
		wc.Timeout <- 60000
		wc.Method <- "POST";
		wc.ContentType <- contentType
	
	
		wc.ContentLength <- postData.LongLength
		use postStream = wc.GetRequestStream()
		postStream.Write(postData,0,postData.Length);
		postStream.Flush();
		postStream.Close();
		wc
	try
		use response = wc.GetResponse()
		use rStream = response.GetResponseStream()
		use r = new StreamReader(rStream,true)
		let rawResults=
			let toDump = r.ReadToEnd()
			toDump.Dump("ResponseStream")
			toDump
		Newtonsoft.Json.JsonConvert.DeserializeObject<UpdateQuotaGroupResponseF>(rawResults)
	with 
	| :? WebException as ex -> 
		use rStream = ex.Response.GetResponseStream()
		use r = new StreamReader(rStream,true)
		[
			"raw",raw :> obj
			"response content",upcast r.ReadToEnd()
			"ex", upcast ex
			
			].Dump()
		failwith "response exception"
		
(raw,updateQg postData contentType).Dump("request+response")
printf "before, after ->"
[
	before
	getQg()
]
|> Dump

