<Query Kind="FSharpProgram" />

open System.Net.Http
type TwitterProfileInfo = 
	{ 
		Screen_Name:string
		User_Id:string
		LastUpdate:DateTime
		Followers:int
		Friends:int
		Country:string
		Gender:string // string option also works
		Interests:string[]
		Profiles:string[]
		PeerGroups:string[]
	}
	
let sample = """{"screen_name":"MaslowJax","user_id":"155035530","lastupdate":"2012-08-16 13:50:50","followers":"22","friends":"102","country":"us","gender":null,"interests":[],"profiles":["webtech","developer"],"peergroups":[]}"""

let getFromApi debug (key:string) sn = 
	use client=new HttpClient()
	client.DefaultRequestHeaders.Add("X-Mashape-Key",key);
	let json = client.GetStringAsync("https://peerreach.p.mashape.com/user/lookup.json?screen_name=" + sn).Result;
	if debug then json.Dump("raw");
	json
//Newtonsoft.Json.JsonConvert.DeserializeObject(json).Dump("object");
let toTwitterProfileInfo debug input = 
	let result = Newtonsoft.Json.JsonConvert.DeserializeObject<TwitterProfileInfo>(input)
	if debug then result.Dump("classy");
	result

toTwitterProfileInfo false sample |> ignore
toTwitterProfileInfo true <| getFromApi true (Util.GetPassword("X-Mashape-Key")) "maslowjax" |> ignore
