<Query Kind="FSharpProgram" />

open System.Net.Http
let uri lat long apiKey = sprintf "https://maps.googleapis.com/maps/api/geocode/json?latlng=%f,%f&key=%s" lat long apiKey
let linq uri = new Hyperlinq(uri)
let linqf (uri:string) text = new Hyperlinq(uri,text)
let map lat lon = linq <| sprintf "http://maps.google.com/?q=%f,%f" lat lon

let getFromApi debug (mashapekey:string option) (uri:string) = 
	use client=new HttpClient()
	if mashapekey.IsSome then client.DefaultRequestHeaders.Add("X-Mashape-Key",mashapekey.Value);
	let json = client.GetStringAsync(uri).Result;
	if debug then json.Dump("raw");
	json

//Util.SetPassword("Google+ApiKey",Util.ReadLine("password"))
let lat,lon = (34.75, -77.43)
let mappedUri = uri lat lon (Util.GetPassword("Google+ApiKey"))
(map lat lon).Dump("mapLink")
mappedUri.Dump("uri")
let json = getFromApi false None mappedUri
type AddressComponent = {
	long_name:string
	short_name:string
	types:List<string>
	}
type Location = {
	lat:double
	lng:double
	}
type Northeast = {
	lat:double
	lng:double
	}
type Southwest = {
	lat:double
	lng:double
	}
type Viewport = {
	northeast:Northeast
	southwest:Southwest
}
type Geometry = {
	location:Location
	location_type:string
	viewport:Viewport
}
type Result = {
	address_components:List<AddressComponent>
	formatted_address:string
	geometry:Geometry
	types:List<string>
}
type RootObject = {
	results:List<Result>
	status:string
}

Newtonsoft.Json.JsonConvert.DeserializeObject<RootObject>(json).Dump("recorded")

