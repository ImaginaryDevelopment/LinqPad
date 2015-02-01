<Query Kind="FSharpProgram" />

open System.Net.Http
[<Measure>]
type Latitude
[<Measure>]
type Longitude
type Coord = { Lon: float<Longitude>; Lat:float<Latitude>}
	with 
	member x.GetGoogleMapLink() = 
		sprintf "http://maps.google.com/?q=%f,%f" <| float x.Lat <| float x.Lon
		
type Sys = { Message:string; Country:string; Sunrise:int; Sunset:int}
type Weather = { Id:int; Main:string; Description:string; Icon:string}
type Main = { Temp:decimal; Humidity:decimal; Pressure:string;Temp_Min: decimal; Temp_Max:decimal}
type Wind = { Speed:decimal; Gust:decimal; Deg:decimal}

type WeatherApiInfo = 
	{ 
		Coord:Coord
		Sys:Sys
		Weather:Weather list
		Base:string
		Main:Main
	}
	
let sample = """{"coord":{"lon":-77.43,"lat":34.75},"sys":{"message":0.0286,"country":"US","sunrise":1422792456,"sunset":1422830345},"weather":[{"id":804,"main":"Clouds","description":"overcast clouds","icon":"04d"}],"base":"cmc stations","main":{"temp":283.418,"temp_min":283.418,"temp_max":283.418,"pressure":1035.94,"sea_level":1037.37,"grnd_level":1035.94,"humidity":38},"wind":{"speed":1.61,"deg":196.001},"clouds":{"all":92},"dt":1422810901,"id":4473083,"name":"Jacksonville","cod":200}"""

let getFromApiLatLong debug (key:string) lang (lat:float<Latitude>) (long:float<Longitude>) = 
	use client=new HttpClient()
	client.DefaultRequestHeaders.Add("X-Mashape-Key",key);
	let uri = sprintf "https://community-open-weather-map.p.mashape.com/weather?lang=%s&lat=%f&lon=%f" lang <| float lat <| float long
	uri.Dump("uri")
	let json = client.GetStringAsync(uri).Result;
	if debug then json.Dump("raw");
	json
	
let getFromApi debug (key:string) lang q  = 
	use client=new HttpClient()
	client.DefaultRequestHeaders.Add("X-Mashape-Key",key);
	let uri = sprintf "https://community-open-weather-map.p.mashape.com/weather?lang=%s&q=%s" lang q
	uri.Dump("uri")
	let json = client.GetStringAsync(uri).Result;
	if debug then json.Dump("raw");
	json
//Newtonsoft.Json.JsonConvert.DeserializeObject(json).Dump("object");
let toTypedResult debug input = 
	let result = Newtonsoft.Json.JsonConvert.DeserializeObject<WeatherApiInfo>(input)
	if debug then result.Dump("classy");
	result

// try to deserialize the sample first to see if it is working
let sampleInfo = toTypedResult false sample
sampleInfo.Coord.GetGoogleMapLink().Dump()
"attempting real api call".Dump();
let byLatLong = toTypedResult true <| getFromApiLatLong true (Util.GetPassword("X-Mashape-Key")) "en" 25.77<Latitude> -80.19<Longitude>
let info = toTypedResult true <| getFromApi true (Util.GetPassword("X-Mashape-Key")) "en" "miami"

