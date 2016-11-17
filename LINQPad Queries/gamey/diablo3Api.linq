<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.FSharp</Namespace>
  <Namespace>Newtonsoft.Json</Namespace>
</Query>

//following https://github.com/stephenpoole/d3-leaderboards-api
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
    let tryGetUrl headers (url:string) = 
        use hc = new System.Net.Http.HttpClient()
        hc.DefaultRequestHeaders.Add("Accept", "application/json")
        
        try
            hc.GetStringAsync(url).Result
            |> Dump
            |> ignore
        with ex ->
            url.Dump()
            ex.Dump()
            
    let getToken key secret =
        tryGetUrl ["client_id",key; "client_secret", secret; "grant_type", "client_credentials"] 
    
module BattleNet =
    let url = "https://us.api.battle.net/"
    let getSavedApiKey() = Util.GetPassword("Diablo3ApiKey")
    let getSavedAccessToken() = Util.GetPassword("Diablo3AccessToken")
    
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
        HttpClient.tryGetUrl List.empty (sprintf "https://us.api.battle.net/d3/data/follower/templar?apikey=%s" key)
    let getSampleDataTest() = // works
        getSampleData (getSavedApiKey())
        |> Dump        
    let getSampleSeasonDataTest() = 
        HttpClient.tryGetUrl List.empty (sprintf "https://us.api.battle.net/data/d3/season?apikey=%s" (getSavedApiKey()))
    let getSeasonIndex () (* accessKey *) = 
        sprintf "https://us.api.battle.net/data/d3/season/?access_token=%s" (getSavedAccessToken())
        |> HttpClient.tryGetUrl List.Empty
    let getSeasonInfo () = 
        sprintf "https://us.api.battle.net/data/d3/season/7?namespace=2-1-US&access_token=%s" (getSavedAccessToken())
        |> HttpClient.tryGetUrl List.empty
        
        
module D3LeaderboardsApi = 
    let getSoloList dclass era = 
        let url = sprintf "http://us.battle.net/d3/en/rankings/season/%i/rift-%O" era dclass
        HttpClient.tryGetUrl ["Accept", "application/json"] url
        
    //getSoloList DemonHunter 7
//D3BattleNetApi.getSampleSeasonData()
//|> Dump
open D3Api
getSampleDataTest()


// fails unauthorized
getSeasonInfo()
|> Dump