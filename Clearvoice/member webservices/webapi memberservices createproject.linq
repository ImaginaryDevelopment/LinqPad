<Query Kind="Program">
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

void Main()
{
	var useSprocCallToTestAuth = false;
	var host = "http://localhost:18217/";
	
	var orgs = Org_api_logins.Dump().Select(o=>new{o.Org_id,o.Org.Org_name}).Distinct();
	
	var targetOrg =int.Parse(Util.ReadLine("OrgId?","0",orgs.Select(o=>o.Org_id +"("+ o.Org_name+")").ToArray()));
	var orgApiLogin = Org_api_logins.First(oa => oa.Org_id==targetOrg);
	var controller="Projects"; // "SurveyInventory";
	var method ="CreateProject";// "GetSurveysForExternalMember";
	if(useSprocCallToTestAuth){
		var shouldLoginSucceed = OrgApiLogin_Authenticate(orgApiLogin.Username,orgApiLogin.Password,"::1",controller,method).Dump("authenticate");
	
		if(shouldLoginSucceed.Tables.Count<1 || shouldLoginSucceed.Tables[0].Rows.Count<1) throw new ArgumentOutOfRangeException();
	}
	
	var uri = host+"api/"+controller+"/"+method;
	
	var wc = System.Net.WebRequest.Create(uri);
	var authInfo = Convert.ToBase64String(Encoding.Default.GetBytes( orgApiLogin.Username+":"+orgApiLogin.Password));
	wc.Timeout= 60000;
	//wc.Credentials= new NetworkCredential("6D324BD2-BCDE-480C-91F6-C8A1AA9730A5","Test");
	wc.Method="POST";
	wc.ContentType="application/x-www-form-urlencoded";
	
	wc.Headers.Add("Authorization: Basic "+authInfo);
	var postData = BuildPost("Brandon ApiCreateProject"+DateTime.Now.ToString("yyyyMMddhh"),"helloSurveyUrl",5,null,null,null);
	wc.ContentLength = postData.Length;
	using(var postStream = wc.GetRequestStream()){
	
		postStream.Write(postData,0,postData.Length);
		postStream.Flush();
		postStream.Close();
	}
	
	try
	{	        
		using(var response=wc.GetResponse()){
			using(var rStream = response.GetResponseStream())
			using(var r = new StreamReader(rStream,true)){
				var rawResults=r.ReadToEnd().Dump();
				Newtonsoft.Json.JsonConvert.DeserializeAnonymousType(rawResults,new{ ExtMembersSurveys=new []{new {ProjectQuotaGuid=Guid.Empty,SurveyName="", SurveyUrl="",SurveyLength=5,Revenue=(decimal?)0.1m, IncidenceRate=(decimal?)0.01m}}}).Dump();;
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
}
public byte[] BuildPost(string name, string surveyurl, int surveyLength, int? externalSurveyId, string partnerSalesperson, decimal? requestedCpi){
	
	
}
// Define other methods and classes here
public byte[] BuildPost(string name, string surveyurl, int surveyLength, int? externalSurveyId, string partnerSalesperson, decimal? requestedCpi){
	var sb = new StringBuilder();
	
	sb.Append("Name="+HttpUtility.UrlEncode(name)+"&");
	sb.Append("SurveyUrl="+HttpUtility.UrlEncode(surveyurl)+"&");
	sb.Append("SurveyLength="+surveyLength.ToString()+"&");
	sb.Append("Triggers.Starts="+54321);
	
	var encoding = new ASCIIEncoding();
	return encoding.GetBytes(sb.ToString());
}