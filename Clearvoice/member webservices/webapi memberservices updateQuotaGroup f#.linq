<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>System.Net</Namespace>
  <Namespace>System.Web</Namespace>
</Query>

type PartialUpdate<'a> =
	| Ignore
	| Clear
	| Update of 'a
	
type UpdateQuotaGroupRequestF = {
		QuotaGroupGuid:Guid
		QuotaGroupName:PartialUpdate<String>
		Closed:Nullable<bool>
		RequestedCompletes:Nullable<int>
		BidResponseRate:Nullable<decimal>
		IncidenceRate:Nullable<decimal>
		AllowDynamicSurveys:Nullable<bool>
		PanelSources: int list
		CompleteRewardOverride:Nullable<decimal>
		TerminateRewardOverride:Nullable<decimal>
		OverQuotaRewardOverride:Nullable<decimal>
		AllowExternalSuppliers:Nullable<bool>
	}

type ScriptArgs = {Host:string;PostData:UpdateQuotaGroupRequestF;QgGuid:Guid;UseSprocCallToTestAuth:bool;Controller:string;ApiMethod:string}
let scriptArgs = 
	let qgGuid = System.Guid("f1e55caf-7102-49c7-806e-f24637dcb6e2")
	let randomQuotaGroupName = 
		sprintf "hello api %s" (DateTime.Now.ToString())
	{
	ScriptArgs.Host="http://localhost:18217/"
	
	PostData={
				UpdateQuotaGroupRequestF.QuotaGroupGuid = qgGuid
				QuotaGroupName = Update randomQuotaGroupName
				Closed = Nullable<bool>(false)
				RequestedCompletes = Nullable<int>()
				BidResponseRate = Nullable<decimal>()
				IncidenceRate = Nullable<decimal>()
				AllowDynamicSurveys = Nullable<bool>()
				PanelSources = list.Empty
				CompleteRewardOverride = Nullable<decimal>()
				TerminateRewardOverride = Nullable<decimal>()
				OverQuotaRewardOverride = Nullable<decimal>()
				AllowExternalSuppliers = Nullable<bool>()
		}
	QgGuid = qgGuid
	UseSprocCallToTestAuth = false
	Controller="QuotaGroups" // "SurveyInventory";
	ApiMethod ="UpdateQuotaGroup"// "GetSurveysForExternalMember";
	}


(* **************** end settings section *********************** *)

let dc = new TypedDataContext()

dc.Settings.Context.DeferredLoadingEnabled <- false
dc.Settings.Context.ObjectTrackingEnabled <- false

let getQg()= dc.Quota_groups.First(fun qg-> qg.Project_quota_guid = scriptArgs.QgGuid)	
let getQgm qgId = 
	dc.Quota_group_metas.FirstOrDefault(fun qgm -> qgm.Project_quota_id = qgId)
	
let orgs = 
	let orgApiLogins = query {
		for oal in dc.Org_api_logins do
		join org in dc.Orgs on
			(oal.Org_id = org.Org_id)
		select (oal.Org_id, org.Org_name)
	}
	orgApiLogins
	|> Seq.distinct 
	
// http://localhost:18217/Help/Api/POST-api-Api.V1.Controllers.QuotaGroups-UpdateQuotaGroup
type PostContent = 
	| FormEncoded
	| Json
	| Xml
	
let BuildPost postType (request:UpdateQuotaGroupRequestF):string*byte[]*string =
	// http://stackoverflow.com/questions/14702902/post-form-data-using-httpwebrequest
	let values:seq<string*obj> = 
		seq {
				yield "QuotaGroupGuid", request.QuotaGroupGuid.ToString():> obj
				match request.QuotaGroupName with
					| Ignore -> ()
					| Clear -> yield "QuotaGroupName",upcast Unchecked.defaultof<string>
					| Update(x) -> yield "QuotaGroupName",upcast  x
				if request.Closed.HasValue then
					yield "Closed", upcast request.Closed
				if request.RequestedCompletes.HasValue then
					yield "RequestedCompletes", upcast request.RequestedCompletes
				if request.BidResponseRate.HasValue then
					yield "BidResponseRate", upcast request.BidResponseRate
			}
	let formEncode () = 
		let sb = StringBuilder()
		let buildPair name value = sprintf "%s=%s" name value
		let encode (v:string) = HttpUtility.UrlEncode(v)
		let pairs = 
			values
			|> Seq.map (fun (name,value) ->
				match value with 
				//| ?: int as i -> buildPair(name,value)
				| :? string as s -> 
					buildPair name (encode(s))
				| _ -> buildPair name (value.ToString())
				)
			|> Seq.reduce (fun x y -> sprintf "%s&%s" x y)
		sb.Append(pairs) |> ignore
		sb.ToString(),"application/x-www-form-urlencoded"
	let jsonEncode() = 
		Newtonsoft.Json.JsonConvert.SerializeObject <| (values.ToDictionary((fun s->fst s),(fun s-> snd s))),
		"application/json"
	let text,contentType = match postType with
							| Json -> jsonEncode()
							| FormEncoded -> formEncode()
	let encoding = new ASCIIEncoding()
	text,encoding.GetBytes(text),contentType


let raw,postData,contentType = BuildPost Json scriptArgs.PostData

orgs.Dump("orgApiLogins") // display to user so that the readLine input is easier to do
	
let targetOrg =
	let orgMap = orgs |> Seq.map (fun (id,name) -> id)
	Util.ReadLine<int>("OrgId?",0, orgMap)
let orgApiLogin = dc.Org_api_logins.First(fun oa -> oa.Org_id=targetOrg)

if scriptArgs.UseSprocCallToTestAuth then 
	let shouldLoginSucceed = dc.OrgApiLogin_Authenticate(orgApiLogin.Username,orgApiLogin.Password,"::1",scriptArgs.Controller,scriptArgs.ApiMethod)
	shouldLoginSucceed.Dump("authenticate")
	
	if shouldLoginSucceed.Tables.Count<1 || shouldLoginSucceed.Tables.[0].Rows.Count<1 then  failwith "login will probably fail"
	
let uri = scriptArgs.Host+"api/"+scriptArgs.Controller+"/"+scriptArgs.ApiMethod


let before = getQg()
let beforeMeta = getQgm before.Project_quota_id
type UpdateQuotaGroupResponseF = { QuotaGroupGuid:System.Guid;}	


let updateQg (postData:byte[]) contentType = 
	let webClient = 
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
		use response = webClient.GetResponse()
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

type RawReqResponse = {Raw:string;Response:UpdateQuotaGroupResponseF}		
{Raw=raw;Response=updateQg postData contentType}.Dump("request+response")
printf "before, after ->"
[ before ; 	getQg() ] |> Dump
[ beforeMeta; 	getQgm(before.Project_quota_id) ] |> Dump
