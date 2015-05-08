<Query Kind="Program">
  <Reference>C:\Development\Products\CVS\Member\CVS.Member.WebServices\bin\CVS.Member.WebServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Framework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Tasks.v4.0.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Utilities.v4.0.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.ComponentModel.DataAnnotations.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Configuration.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Design.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.Protocols.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.EnterpriseServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.Caching.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Security.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.ServiceProcess.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.ApplicationServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.RegularExpressions.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.Services.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>Microsoft.AspNet.WebApi.Core</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>System.Net</Namespace>
  <Namespace>System.Net.Http</Namespace>
  <Namespace>System.Web</Namespace>
  <Namespace>System.Net.Http.Formatting</Namespace>
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
	
	var wc = new HttpClient();
	wc.BaseAddress= new Uri(uri);
	var authInfo = Convert.ToBase64String(Encoding.Default.GetBytes( orgApiLogin.Username+":"+orgApiLogin.Password));
	//wc.Timeout= 60000;
	//wc.Credentials= new NetworkCredential("6D324BD2-BCDE-480C-91F6-C8A1AA9730A5","Test");
	wc.DefaultRequestHeaders.Add("Authorization","Basic "+authInfo);
	 var request = new CVS.Member.WebServices.Api.V1.Models.Projects.CreateProjectRequest()
            {
               ProjectName = "LinqPadCreateTest" + DateTime.Now.ToString("yyyyMMddHHmmss"),
               SurveyUrl = "testSurveyUrl",
               SurveyLength = 5
            };
	wc.PostAsync(string.Empty,request,new JsonMediaTypeFormatter()).Result.Dump();
	
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