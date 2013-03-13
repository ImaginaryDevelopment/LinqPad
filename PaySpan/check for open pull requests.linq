<Query Kind="Program">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
</Query>

void Main()
{
var urlHpxC = "http://jaxscm1.payformance.net:7990/rest/api/1.0/projects/DEVC/repos/hpx-current/pull-requests?state=OPEN";
var urlPciC = "http://jaxscm1.payformance.net:7990/rest/api/1.0/projects/DEVC/repos/pci-current/pull-requests?state=OPEN";
var urlHpxN = "http://jaxscm1.payformance.net:7990/rest/api/1.0/projects/DEVN/repos/hpx-next/pull-requests?state=OPEN";
var urlPciN = "http://jaxscm1.payformance.net:7990/rest/api/1.0/projects/DEVN/repos/pci-next/pull-requests?state=OPEN";
var urlHpxP = "http://jaxscm1.payformance.net:7990/rest/api/1.0/projects/PROD/repos/hpx-production/pull-requests?state=OPEN";
var urlPciP = "http://jaxscm1.payformance.net:7990/rest/api/1.0/projects/PROD/repos/pci-production/pull-requests?state=OPEN";
var addresses= new[]{urlHpxC,urlPciC,urlHpxN,urlPciN,urlHpxP,urlPciP};
	//var address=Util.ReadLine("Address?","http://jaxscm1.payformance.net:7990/rest/api/1.0/projects/DEVC/repos/hpx-current/pull-requests?state=OPEN");
	foreach(var address in addresses)
	{
		var uri=new Uri(address);
		using(var wc = new System.Net.WebClient())
		{
			wc.Headers.Add("Authorization","Basic "+ Convert.ToBase64String(Encoding.ASCII.GetBytes("ApiUser:"+Util.GetPassword("atlassian",true))));
	
			var info=wc.DownloadString(uri);
			
			if(string.IsNullOrWhiteSpace(info))
			{
				("There are 0 open pull requests that need to be done in "+address).BoldRed().Dump();
				continue;
			}
			//	info.Dump("raw");
			var response=Newtonsoft.Json.JsonConvert.DeserializeObject<IDictionary<string,dynamic>>(info);
			foreach(dynamic v in response["values"])
			{
				var raw= ((Newtonsoft.Json.Linq.JArray)v.reviewers).ToArray().Cast<dynamic>();
					var reviewers=from d in raw.ToArray()
					select ((string)(d.user.name.ToString())).StyleAs("color:orange");
				var id=(string)v.id;
				var rawParticipants=wc.DownloadString(address.Before("?")+"/"+id+"/participants").Dump("raw participants");
				var participants=Newtonsoft.Json.JsonConvert.DeserializeObject<IDictionary<string,dynamic>>(rawParticipants);
				//participants.Dump("participants");
				var completeCount=-1;
				var notApproved= new List<string>();
				foreach(dynamic p in participants["values"])
				{
					if(completeCount<0) completeCount++;
					if((bool)p.approved) {completeCount++;} else {
					var name=(string)p.user.name;
					var role=(string)p.role;
					//new{ name,role}.Dump("name/role");
					if(role != "AUTHOR")
						notApproved.Add((string)p.user.name);
					}
				}
				var x = new {
					Author=((string)v.author.user.displayName.ToString()).BoldRed(),
					Title=((string)v.title).BoldRed(),
					Description=((string)(v.description)).StyleAs("color:orange"),
					State=((string)(v.state)).StyleAs("color:orange"),
					Url=((string)(v.fromRef.repository.link.url)).StyleAs("color:orange"),
					SuggestedReviewers=reviewers.ToArray(),
					PullRequestId=id,
					CompleteCount=completeCount,
					Remaining=notApproved.ToArray()
					};
				
				
				//participants.Dump();
				x.Dump();
				
			}
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