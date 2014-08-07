<Query Kind="Statements">
  <Connection>
  </Connection>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>System.Net</Namespace>
</Query>

var useSprocCallToTestAuth = false;
var host = "http://localhost:18217/";

var orgs = Org_api_logins.Dump().Select(o=>new{o.Org_id,o.Org.Org_name}).Distinct();
Ip_whitelists.Dump();
var targetOrg =int.Parse(Util.ReadLine("OrgId?","0",orgs.Select(o=>o.Org_id +"("+ o.Org_name+")").ToArray()));
var orgApiLogin = Org_api_logins.First(oa => oa.Org_id==targetOrg);
var controller="Project"; // "SurveyInventory";
var method ="CreateProject";// "GetSurveysForExternalMember";
if(useSprocCallToTestAuth){
	var shouldLoginSucceed = OrgApiLogin_Authenticate(orgApiLogin.Username,orgApiLogin.Password,"::1",controller,method).Dump("authenticate");

	if(shouldLoginSucceed.Tables.Count<1 || shouldLoginSucceed.Tables[0].Rows.Count<1) throw new ArgumentOutOfRangeException();
}
var externalMemberId="24507f3c-4fe4-46eb-816d-d0c031629231"; //"MATTEST1"; //"3709277";
var uri = host+"api/"+controller+"/"+method+"?ExternalMemberId="+externalMemberId;

var wc = System.Net.WebRequest.Create(uri);
var authInfo = Convert.ToBase64String(Encoding.Default.GetBytes( orgApiLogin.Username+":"+orgApiLogin.Password));
wc.Timeout= 60000;
//wc.Credentials= new NetworkCredential("6D324BD2-BCDE-480C-91F6-C8A1AA9730A5","Test");

wc.ContentType="application/json";

wc.Headers.Add("Authorization: Basic "+authInfo);
try
{	        
	using(var response=wc.GetResponse()){
		using(var rStream = response.GetResponseStream())
		using(var r = new StreamReader(rStream,true)){
			var rawResults=r.ReadToEnd().Dump();
			Newtonsoft.Json.JsonConvert.DeserializeAnonymousType(rawResults,new{ ExtMembersSurveys=new []{new {ProjectQuotaGuid=Guid.Empty,SurveyName="", SurveyUrl="",SurveyLength=5,Revenue=(decimal?)0.1m, IncidenceRate=(decimal?)0.01m}}}).Dump();
		}
	}	
}
catch (WebException ex)
{
using (var rStream = ex.Response.GetResponseStream())
using(var r = new StreamReader(rStream,true)){
r.ReadToEnd().Dump("response ex");
}
	ex.Dump();
}