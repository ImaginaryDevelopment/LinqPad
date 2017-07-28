<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.FSharp</Namespace>
  <Namespace>Newtonsoft.Json</Namespace>
</Query>

//following https://github.com/stephenpoole/d3-leaderboards-api
open Newtonsoft.Json.Linq
module Helpers =
    let dumpt t x = x.Dump(description=t); x
    let dumptd condition t x = if condition then dumpt t x else x
    let deserializeJO x = Newtonsoft.Json.JsonConvert.DeserializeObject<JObject>(x)
    let toLower (x:string) = x.ToLower()
    // stopping linqpad is crashing perhaps
    let clipAsText x = (x |> string |> System.Windows.Forms.Clipboard.SetText); x
open Helpers
module Diablo3 =
    type DClass = 
        | DemonHunter
        | Barbarian
        | WitchDoctor
        | Wizard
        | Monk
        | Crusader
        | Necromancer
        with 
            override x.ToString() = 
                match x with | DemonHunter -> "dh" | WitchDoctor -> "wd" | _ -> sprintf "%A" x |> toLower
                |> fun x -> x.ToLower()
    let eras = [1;2;3;4;5;6;7]
    let teams = [2;3;4]
    
module HttpClient = 
    // not accounting for needing to escape & nor encoding other characters in the querystring
    let buildQueryString items = 
        items
        |> Seq.map(fun (k,v) -> sprintf "%s=%s" k v) 
        |> delimit "&"

    let tryGetUrl debug (url:string) = 
        use hc = new System.Net.Http.HttpClient()
        hc.DefaultRequestHeaders.Add("Accept", "application/json")
        
        try
            let result = hc.GetStringAsync(url).Result
            if debug then
                result.Dump("tryGetUrl debug")
            result
            |> Some
            //|> ignore
        with ex ->
            (url,ex).Dump("failure")
            None
            
    let getToken key secret =
        ["client_id",key; "client_secret", secret; "grant_type", "client_credentials"]
        |> buildQueryString
        |> sprintf "https://us.battle.net/oauth/token?%s"
        |> tryGetUrl false
    
module BattleNet =
    let url = "https://us.api.battle.net/"
    // key and secret at https://dev.battle.net/apps/mykeys
    let getSavedApiKey() = Util.GetPassword("Diablo3ApiKey")
    let getSavedSecret() = Util.GetPassword("Diablo3ApiSecret")
    type AccessTokenInfo = {Access_Token:string;Token_Type:string; Expires_In:int}
    let getAccessToken() = // worked
        let getter() = 
            HttpClient.getToken (getSavedApiKey()) (getSavedSecret())
            |> dumpt "getAccessToken"
        Util.Cache(getter,"Diablo3AccessToken")
        // don't stick this part in the getter, suspect caching won't like object vs string
        |> Option.map (fun text ->
                JsonConvert.DeserializeObject<AccessTokenInfo>(text)
                |> fun x -> x.Access_Token
        )

module D3Api = 
    (* 
        flow: get api key/secret
        Authorize -> Auth code
        GetAccessToken -> access token
    *)
    open BattleNet
    open Diablo3
    
    let testUrl = "https://us.api.battle.net/data/d3/season/7"
    
    let getSampleData key = 
        HttpClient.tryGetUrl false (sprintf "https://us.api.battle.net/d3/data/follower/templar?apikey=%s" key)
    let getSampleDataTest() = // works
        getSampleData (getSavedApiKey())
        |> dumpt "getSampleDataTest"
    let getSampleSeasonDataTest() = 
        HttpClient.tryGetUrl false (sprintf "https://us.api.battle.net/data/d3/season?apikey=%s" (getSavedApiKey()))
        |> dumpt "getSampleSeasonDataTest"
    type SeasonRef = {Href:string}
    type SeasonIndex = { 
        // only contains url to get the data we already have, string failed
        //_Links: Newtonsoft.Json.Linq.JRaw
        Current_Season:int
        Last_Update_Time:string
        Season: SeasonRef[]
    }
    let cachedGetMap<'T> name debug url : ('T*obj) option=
        let getter() = 
            HttpClient.tryGetUrl debug url
        Util.Cache(getter,name)
        |> Option.map (dumptd debug name)
        |> Option.map (fun text -> JsonConvert.DeserializeObject<'T>(text), box (Util.OnDemand("raw", fun () -> text)))
        |> dumptd debug (sprintf "%sMapped" name)
    let getSeasonIndex () (* accessKey *) = // works
        sprintf "https://us.api.battle.net/data/d3/season/?access_token=%s" (getAccessToken().Value)
        |> cachedGetMap<SeasonIndex> "getSeasonIndex" false
    type SeasonLeaderboardRef ={
        Ladder: SeasonRef
        Team_Size: int Nullable
        Hero_Class_String: string
        Hardcore:bool Nullable
    }
    type SeasonInfo = {
        Season_Id:int
        Last_Update_Time:string
        Generated_By: string
        Leaderboard: SeasonLeaderboardRef[]
        Raw: obj
    }
    // working
    let getSeasonInfo season = 
        let ``d3 url namespace`` = "2-6-US"
        sprintf "https://us.api.battle.net/data/d3/season/%i?namespace%s&access_token=%s" season ``d3 url namespace`` (getAccessToken().Value)
        |> cachedGetMap<SeasonInfo> "getSeasonInfo" false
        |> Option.map (fun (x,raw) -> {x with Raw = raw})
    type DataElement = {
        Id:string
        Number: int64 Nullable
        String: string
        Timestamp: int64 Nullable
    }
    type SLDetailPlayer = {
        Key:string;
        AccountId: int64 Nullable
        Data: DataElement[]
    }
    type SLDetailedRow = {
        Player: SLDetailPlayer[]
        Order:int
        Data: DataElement[]
    }
    type SLDetailed = {
        Row: SLDetailedRow[]
        Season:int;
        Greater_Rift_Solo_Class: string
        // don't know if truly optional
        Greater_Rift:bool Nullable
        Last_Update_Time:string;
        Column: JRaw[]
        Title: JObject
        Key: string
    }
let getUriForDump url f = 
    Util.OnDemand(url, 
            fun () -> 
                // assuming they included the query string with namespace already, adding access token
                sprintf "%s&access_token=%s" url (BattleNet.getAccessToken().Value)
                |> HttpClient.tryGetUrl true 
                |> f)
open D3Api
getSeasonIndex()
|> ignore
printfn "getting seasonInfo"
getSeasonInfo 11
//|> dumpt "Season Leaderboards whole"
|> Option.map (fun x -> x.Leaderboard)
|> Option.map (Array.filter (fun l -> l.Hardcore |> Nullable.getValueOrDefault |> not || l.Team_Size |> Nullable.getValueOrDefault > 1))
// working
|> Option.map (Array.map (fun l -> l.Hero_Class_String, getUriForDump l.Ladder.Href (clipAsText >> Option.map (fun text -> JsonConvert.DeserializeObject<SLDetailed> text))))
|> dumpt "Season Leaderboards"
|> ignore