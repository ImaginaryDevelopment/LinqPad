<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.FSharp</Namespace>
  <Namespace>Newtonsoft.Json</Namespace>
</Query>

//following https://github.com/stephenpoole/d3-leaderboards-api
open Newtonsoft.Json.Linq
module Helpers =
    let dumpt t x = x.Dump(description=t); x
    let deserializeJO x = Newtonsoft.Json.JsonConvert.DeserializeObject<JObject>(x)
open Helpers
module Diablo3 =
    type DClass = 
        | DemonHunter
        | Barbarian
        | WitchDoctor
        | Wizard
        | Monk
        | Crusader
        with 
            override x.ToString() = 
                match x with | DemonHunter -> "dh" | WitchDoctor -> "witch-doctor" | _ -> sprintf "%A" x
                |> fun x -> x.ToLower()
    let eras = [1;2;3;4;5;6;7]
    let teams = [2;3;4]
    
module HttpClient = 
    // not accounting for needing to escape & nor encoding other characters in the querystring
    let buildQueryString items = 
        items
        |> Seq.map(fun (k,v) -> sprintf "%s=%s" k v) 
        |> delimit "&"

    let tryGetUrl (url:string) = 
        use hc = new System.Net.Http.HttpClient()
        hc.DefaultRequestHeaders.Add("Accept", "application/json")
        
        try
            hc.GetStringAsync(url).Result
            |> Dump
            |> Some
            //|> ignore
        with ex ->
            (url,ex).Dump("failure")
            None
            
    let getToken key secret =
        ["client_id",key; "client_secret", secret; "grant_type", "client_credentials"]
        |> buildQueryString
        |> sprintf "https://us.battle.net/oauth/token?%s"
        |> tryGetUrl  
    
module BattleNet =
    let url = "https://us.api.battle.net/"
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
        HttpClient.tryGetUrl (sprintf "https://us.api.battle.net/d3/data/follower/templar?apikey=%s" key)
    let getSampleDataTest() = // works
        getSampleData (getSavedApiKey())
        |> dumpt "getSampleDataTest"
    let getSampleSeasonDataTest() = 
        HttpClient.tryGetUrl (sprintf "https://us.api.battle.net/data/d3/season?apikey=%s" (getSavedApiKey()))
        |> dumpt "getSampleSeasonDataTest"
    type SeasonRef = {Href:string}
    type SeasonIndex = { 
        // only contains url to get the data we already have, string failed
        //_Links: Newtonsoft.Json.Linq.JRaw
        Current_Season:int
        Last_Update_Time:string
    }
    
    let getSeasonIndex () (* accessKey *) = // works
        let getter () = 
            sprintf "https://us.api.battle.net/data/d3/season/?access_token=%s" (getAccessToken().Value)
            |> dumpt "getSeasonIndexUrl"
            |> HttpClient.tryGetUrl
        Util.Cache(getter,"Diablo3SeasonIndex")
        |> dumpt "getSeasonIndex"
        |> Option.map (fun text -> JsonConvert.DeserializeObject<SeasonIndex>(text))
        |> dumpt "getSeasonIndexMapped"

    // working
    let getSeasonInfo () = 
        sprintf "https://us.api.battle.net/data/d3/season/7?namespace=2-6-US&access_token=%s" (getAccessToken().Value)
        |> HttpClient.tryGetUrl
        |> dumpt "getSeasonInfo"
        
        
module D3LeaderboardsApi = 
    // not working
    let getSoloList dclass era = 
        let url = sprintf "http://us.battle.net/d3/en/rankings/season/%i/rift-%O" era dclass
        HttpClient.tryGetUrl url
        
open D3Api
getSeasonIndex()
|> ignore

getSeasonInfo()
|> ignore