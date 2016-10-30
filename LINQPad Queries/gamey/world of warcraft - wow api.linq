<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.FSharp</Namespace>
  <Namespace>Newtonsoft.Json</Namespace>
</Query>

//following https://dev.battle.net/io-docs

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
    (* 
        flow: get api key/secret
        Authorize -> Auth code
        GetAccessToken -> access token
    *)

        
module Warcraft =
    open BattleNet
    let getPets (key:string) (realm:string) (characterName:string) (fields:string) (locale:string) (jsonp:string) =
        let url = sprintf "%swow/character/%s/%s?fields=pets&locale=%s&apikey=%s" BattleNet.url realm characterName locale key
        url
        |> HttpClient.tryGetUrl List.empty
open BattleNet        
Warcraft.getPets (getSavedApiKey()) "rexxar" "mmsheep" "pets" "en-US" null
