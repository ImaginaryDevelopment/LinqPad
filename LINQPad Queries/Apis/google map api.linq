<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>Uber.SDK</NuGetReference>
  <Namespace>Newtonsoft.Json</Namespace>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
  <Namespace>Uber.SDK</Namespace>
</Query>

module Helpers =
    let flip f x y = f y x
    let deserialize<'t> x = JsonConvert.DeserializeObject<'t>(x)
    let replace d r =
        function
        | null | "" as x -> x
        | x -> x.Replace(oldValue=d,newValue=r)
    let cache key f = Util.Cache(Func<_> f,key)
    let delimit d items = String.Join(d,value=Array.ofSeq items)
    type FullPath = FullPath of string
    let writeText (FullPath s) txt =
        File.WriteAllText(s,txt)
open Helpers

module Tuple2 =
    let mapSnd f (a,b) = a, f b
module Json =
    let getProperties (x:JObject) =
        x.Properties()
        |> Seq.map(fun jp ->
            jp.Name, jp.Value
        )
    let getProperty name (x:JObject) =
        x.Property(name)
        |> Option.ofObj
        |> Option.map(fun x -> x.Value)
        |> Option.bind Option.ofObj
    let asJObj (x:JToken) = x :?> JObject    
    let asJArray (x:JToken) = x :?> JArray
    let deserializeJObject x = deserialize<JObject> x
module GMap =        
    type Gps = {Lat:decimal;Lng:decimal}
    let gKey =Util.GetPassword("googleAPI")
    let generateMarker name {Lat=lat;Lng=lng} =
        let cleanedVarName = name |> replace " " "_" |> replace "'" "\""
        sprintf "var marker%s = new google.maps.Marker({position: {lat: %f, lng:%f},map:map,title:'%s'});" cleanedVarName lat lng name
    let getWhereAmI key address =
        let target =
            address
            |> replace " " "+"
            |> flip (sprintf @"https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s") key
        target
    let getGps json =
        let walkGeometry (x:JObject) =
//            let oldBoundsWalk =
//                x
//                |> Json.getProperty "bounds"
//                |> Option.map(Json.asJObj >> Json.getProperties >> Seq.map snd >> Seq.map Json.asJObj >> Seq.collect Json.getProperties)
//                |> Option.defaultValue Seq.empty
//                |> Seq.map(Tuple2.mapSnd(string >> Decimal.Parse))
//                |> Seq.fold (fun gps (name,v) ->
//                    (name,v).Dump("something here")
//                    match name with
//                    | "lat" -> {gps with Lat = if gps.Lat = 0m then v else (gps.Lat + v) /2m }
//                    | "lng" -> {gps with Lng = if gps.Lng = 0m then v else (gps.Lng + v) /2m}
//                    | _ -> invalidOp <| sprintf "unknown gps property %s" name
//                ) {Lat=0m;Lng=0m}
            Json.getProperty("location") x
            |> Option.map(Json.asJObj >> fun jp -> 
                {Lat=jp.Property("lat").Value |> string |> Decimal.Parse;Lng=jp.Property("lng").Value |> string|> Decimal.Parse}
            )
        
        json
        |> Json.getProperty "results"
        |> Option.map Json.asJArray
        |> function 
            |Some r -> printfn "found array"; Some r
            | None -> None
        |> Option.bind (fun x -> x.[0] :?> JObject |> Json.getProperty "geometry")
        |> function
            |Some g -> printfn "Found geometry"; Some g
            | None -> None
        |> Option.bind (fun x -> x :?> JObject |> walkGeometry)
        |> function
            |Some b -> printfn "Found bounds"; Some b
            |None -> None
    
module Async =
    let map f asyncItem =
        async{
            let! x = asyncItem
            return f x
        }
    
    let bind f asyncItem =
        async{
            let! x = asyncItem
            let! y = f x
            return y
        }
    let bindTask f asyncItem =
        async{
            let! x = asyncItem
            let! y = Async.AwaitTask <| f x
            return y
        }
        
let fetch' (url:string) =
    printfn "fetching %s" url
    async{
        use wc = new Net.Http.HttpClient()
        let! value = Async.AwaitTask <| wc.GetStringAsync(url)
        return value
    }
let fetch p (url:string) =
    let url = p |> Seq.map(fun (x,y) -> sprintf "%s=%s" x y) |> delimit "&" |> sprintf "%s?%s" url
    fetch' url
let fetchAuth authToken (url:string) =
    async{
        use wc = new Net.Http.HttpClient()
        wc.DefaultRequestHeaders.Add("Authorization",value=sprintf "Bearer %s" authToken)
//        wc.DefaultRequestHeaders.Add("Content-Type","application/json")
        let! x = Async.AwaitTask <| wc.GetStringAsync(url)
        return x
    }
    
let post p (url:string) =
    printfn "Posting to %s" url
    async{
        use wc = new Net.Http.HttpClient()
        use fuec = new Net.Http.FormUrlEncodedContent(p)
        let! result = Async.AwaitTask <| wc.PostAsync(url, fuec)
        return result
    }
    
module Ubering =    
    type DisplayGps = {Display_Name:string;Latitude:decimal;Longitude:decimal}
    [<NoComparison>]
    type BearerResult = 
        |AccessToken of string
        |Error of string * Net.Http.HttpResponseMessage
    type HistoryEntry = {Status:string;Distance:float;Product_Id:string;Start_Time:int64;Start_City:DisplayGps;End_Time:int64;Request_Id:string;Request_Time:int64}
    type HistoryResult = {Count:int;History:HistoryEntry list;Offset:int;Limit:int}
    let getAccessToken ()=
        let data = Map[
                    "client_id","<omitted for version control>"
                    "client_secret",Util.GetPassword("ubersecret")
                    "grant_type", "authorization_code"
                    "code", @"crd.EA.<omitted for version control>"
                    "redirect_uri","http://localhost"
        ]
//        fetch "https://login.uber.com/oauth/v2/authorize?response_type=code&client_id=SbTAG12Fz26uhgNZ6qAxxBTiqabpLKlz&scope=history+history_lite&redirect_uri=http://localhost"
        post (Map.toSeq data |> Seq.map KeyValuePair)  "https://login.uber.com/oauth/v2/token"
        |> Async.bind(fun x -> 
            async{
                let! content = x.Content.ReadAsStringAsync() |> Async.AwaitTask
                if x.IsSuccessStatusCode then
                    return AccessToken content
                else
                    return BearerResult.Error(content,x)
                }
            )
let getHistory bearer =
    fetchAuth bearer "https://api.uber.com/v1.2/history"
    |> Async.RunSynchronously
    |> deserialize<Ubering.HistoryResult>
    |> Dump
    |> ignore
    
    
let sampleData = 
    """{
  "count": 15,
  "history": [
    {
      "status": "completed",
      "distance": 1.4780860317,
      "product_id": "a1111c8c-c720-46c3-8534-2fcdd730040d",
      "start_time": 1475545183,
      "start_city": {
        "latitude": 37.7749,
        "display_name": "San Francisco",
        "longitude": -122.4194
      },
      "end_time": 1475545808,
      "request_id": "fb0a7c1f-2cf7-4310-bd27-8ba7737362fe",
      "request_time": 1475545095
    },
    {
      "status": "completed",
      "distance": 1.2792152568,
      "product_id": "a1111c8c-c720-46c3-8534-2fcdd730040d",
      "start_time": 1475513472,
      "start_city": {
        "latitude": 37.7749,
        "display_name": "San Francisco",
        "longitude": -122.4194
      },
      "end_time": 1475513898,
      "request_id": "d72338b0-394d-4f0e-a73c-78d469fa0c6d",
      "request_time": 1475513393
    },
    {
      "status": "completed",
      "distance": 1.5084526246,
      "product_id": "a1111c8c-c720-46c3-8534-2fcdd730040d",
      "start_time": 1475170251,
      "start_city": {
        "latitude": 37.7749,
        "display_name": "San Francisco",
        "longitude": -122.4194
      },
      "end_time": 1475171154,
      "request_id": "2b61e340-27bd-4937-8304-122009e4a393",
      "request_time": 1475170088
    },
    {
      "status": "completed",
      "distance": 1.4705337758,
      "product_id": "a1111c8c-c720-46c3-8534-2fcdd730040d",
      "start_time": 1475027766,
      "start_city": {
        "latitude": 37.7749,
        "display_name": "San Francisco",
        "longitude": -122.4194
      },
      "end_time": 1475028387,
      "request_id": "58cb7b3c-fe22-47b4-94c0-2cf08b34f4be",
      "request_time": 1475027705
    },
    {
      "status": "completed",
      "distance": 0.6489455763,
      "product_id": "a1111c8c-c720-46c3-8534-2fcdd730040d",
      "start_time": 1475002745,
      "start_city": {
        "latitude": 37.7749,
        "display_name": "San Francisco",
        "longitude": -122.4194
      },
      "end_time": 1475003150,
      "request_id": "57be6f97-e10f-411e-a87e-670011c46b55",
      "request_time": 1475002656
    },
    {
      "status": "completed",
      "distance": 1.3935675129,
      "product_id": "a1111c8c-c720-46c3-8534-2fcdd730040d",
      "start_time": 1474995527,
      "start_city": {
        "latitude": 37.7749,
        "display_name": "San Francisco",
        "longitude": -122.4194
      },
      "end_time": 1474995943,
      "request_id": "c0453d97-4330-4ec2-88ab-38678101cc0b",
      "request_time": 1474995056
    },
    {
      "status": "completed",
      "distance": 1.5046201975,
      "product_id": "a1111c8c-c720-46c3-8534-2fcdd730040d",
      "start_time": 1474909791,
      "start_city": {
        "latitude": 37.7749,
        "display_name": "San Francisco",
        "longitude": -122.4194
      },
      "end_time": 1474910341,
      "request_id": "35822455-e4f5-4339-b763-6fc3ea16dc61",
      "request_time": 1474909743
    },
    {
      "status": "completed",
      "distance": 2.4445998557,
      "product_id": "a1111c8c-c720-46c3-8534-2fcdd730040d",
      "start_time": 1474685017,
      "start_city": {
        "latitude": 37.7749,
        "display_name": "San Francisco",
        "longitude": -122.4194
      },
      "end_time": 1474685568,
      "request_id": "81a0ffda-a879-4443-beb8-e253f4d19ecc",
      "request_time": 1474684872
    },
    {
      "status": "completed",
      "distance": 1.3603866105,
      "product_id": "a1111c8c-c720-46c3-8534-2fcdd730040d",
      "start_time": 1474651767,
      "start_city": {
        "latitude": 37.7749,
        "display_name": "San Francisco",
        "longitude": -122.4194
      },
      "end_time": 1474652253,
      "request_id": "97736867-41ca-432a-b7e9-909e66d833ba",
      "request_time": 1474651636
    }
  ],
  "limit": 10,
  "offset": 0
}""" |> deserialize<Ubering.HistoryResult>
let getSampleHtmlMarker key text= 
    sprintf """
    <!DOCTYPE html>
<html>
  <head>
    <meta name="viewport" content="initial-scale=1.0, user-scalable=no">
    <meta charset="utf-8">
    <title>Simple Markers</title>
    <style>
      /* Always set the map height explicitly to define the size of the div
       * element that contains the map. */
      #map {
        height: 100%%;
      }
      /* Optional: Makes the sample page fill the window. */
      html, body {
        height: 100%%;
        margin: 0;
        padding: 0;
      }
    </style>
  </head>
  <body>
    <div id="map"></div>
    <script>

      function initMap() {
        var myLatLng = {lat: -25.363, lng: 131.044};

        var map = new google.maps.Map(document.getElementById('map'), {
          zoom: 4,
          center: myLatLng
        });
        %s

      }
    </script>
    <script async defer
    src="https://maps.googleapis.com/maps/api/js?key=%s&callback=initMap">
    </script>
  </body>
</html>"""  text key
//sampleData.History
//|> Seq.map(fun x -> GMap.generateMarker x.Start_City.Display_Name {Lat=x.Start_City.Latitude;Lng=x.Start_City.Longitude})
//|> delimit "\r\n        "
//|> getSampleHtmlMarker GMap.gKey
//|> writeText (FullPath @"C:\projects\UberLift\public\markers.html")
//Process.Start @"C:\projects\UberLift\markers.html"
//|> ignore
let getBearer() =
    Ubering.getAccessToken()

let getGpsSample() =
    [
        "DisneyPop", "1050 Century Drive Lake Buena Vista, Florida 32830-8433"
        "Google", "1600 Amphitheatre Parkway in Mountain View, California"
    ]
    |> List.map(fun (key,addr) ->
        key,cache (sprintf "googleApi-%s" key) (fun () ->
            GMap.getWhereAmI GMap.gKey addr
            |> fetch'
            |> Async.RunSynchronously
        )
        |> Json.deserializeJObject
        |> GMap.getGps
    )
    |> Dump
    |> ignore
getGpsSample()