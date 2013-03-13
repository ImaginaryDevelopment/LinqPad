<Query Kind="Program">
  <Connection>
    <ID>e7fc9cc3-407b-41ff-b1c6-74516cabda7c</ID>
    <Persist>true</Persist>
    <Server>jaxpdrbe1</Server>
    <Database>PaySpanHealth_Jobsystem</Database>
  </Connection>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
</Query>

void Main()
{
	//https://developer.atlassian.com/display/FECRUDEV/REST+API+Guide
	var url = "http://jaxreview1:8060/rest-service/reviews-v1/filter/allOpenReviews";


	//var address=Util.ReadLine("Address?","http://jaxscm1.payformance.net:7990/rest/api/1.0/projects/DEVC/repos/hpx-current/pull-requests?state=OPEN");
		var uri=new Uri(url);
		using(var wc = new System.Net.WebClient())
		{
			wc.Headers.Add("Authorization","Basic "+ Convert.ToBase64String(Encoding.ASCII.GetBytes("bdimperio:"+Util.GetPassword("atlassian",true))));
			wc.Headers.Add("Accept","application/json");
	
			var info=wc.DownloadString(uri);
			
			if(string.IsNullOrWhiteSpace(info))
			{
				("There are 0 open pull requests that need to be done in "+url).BoldRed().Dump();
				return;
			}
				info.Dump("raw");
			var response=Newtonsoft.Json.JsonConvert.DeserializeObject<IDictionary<string,dynamic>>(info);
			
			
			foreach(dynamic v in response["reviewData"])
			{
				var id=(string)v.permaId.id;
				var reviewersUrl=url.Before("filter")+id+"/reviewers";
				wc.Headers.Add("Accept","application/json");
				var rawReviewers=wc.DownloadString(reviewersUrl);
				rawReviewers.Dump("Raw reviewers");
				var reviewersResponse=Newtonsoft.Json.JsonConvert.DeserializeObject<IDictionary<string,dynamic>>(rawReviewers);
				var l= new System.Collections.ArrayList();
				foreach(dynamic r in reviewersResponse["reviewer"])
				{
					l.Add(new{Username=(string)r.userName,Completed=(bool)r.completed});
				}
				l.Dump("reviewers formatted");
				var x = new {
					Id=(string)v.permaId.id,
					Name=(string)v.name, 
					Description=(string)v.description,
					Author=(string)v.author.userName,
					Creator=(string)v.creator.userName,
					Created=(string)v.createDate,
					ProjectKey=(string)v.projectKey,
					State=(string)v.state,
					DueDate=(String)v.dueDate};
				x.Dump();
			}
		}
	
}
static class Extensions
{
	public static object StyleAs(this object value, string style)
	{
	return Util.RawHtml("<span style='"+style+"'>"+value.ToString()+"</span>");
	}
	public static object BoldRed (this object value)
	{
		return value.StyleAs("color:red;font-weight: bold");
	}
}