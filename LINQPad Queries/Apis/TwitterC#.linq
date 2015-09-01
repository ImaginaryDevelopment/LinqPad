<Query Kind="Program">
  <Namespace>System.Net.Http</Namespace>
</Query>

void Main()
{
	using(var client=new HttpClient()){
		client.DefaultRequestHeaders.Add("X-Mashape-Key",Util.GetPassword("X-Mashape-Key"));
		var json = client.GetStringAsync("https://peerreach.p.mashape.com/user/lookup.json?screen_name=maslowjax").Result.Dump("raw");
		//Newtonsoft.Json.JsonConvert.DeserializeObject(json).Dump("object");
		
		Newtonsoft.Json.JsonConvert.DeserializeObject<TwitterProfileInfo>(json).Dump("classy");
	}
	
}

// Define other methods and classes here
public class TwitterProfileInfo{
	public string Screen_Name{get;set;}
	public string User_Id{get;set;}
	public DateTime LastUpdate{get;set;}
	public int Followers{get;set;}
	public int Friends {get;set;}
	public string Country{get;set;}
	public string Gender{get;set;}
	public string[] Interests {get;set;}
	public string[] Profiles{get;set;}
	public string[] PeerGroups{get;set;}
	
}