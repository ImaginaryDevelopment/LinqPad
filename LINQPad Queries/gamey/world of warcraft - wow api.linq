<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.FSharp</Namespace>
  <Namespace>Newtonsoft.Json</Namespace>
</Query>

//following https://dev.battle.net/io-docs
let doLaters = ResizeArray<_>()
let prettifyJsonObj o = JsonConvert.SerializeObject(o, Formatting.Indented)
let prettifyJson s = s |> JsonConvert.DeserializeObject |> prettifyJsonObj
module HttpClient = 
    let tryGetUrl headers (url:string) = 
        use hc = new System.Net.Http.HttpClient()
        hc.DefaultRequestHeaders.Add("Accept", "application/json")
        
        try
            hc.GetStringAsync(url).Result
            |> Some
        with ex ->
            url.Dump()
            ex.Dump()
            None
            
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
    
    
    type PetDisplay = {Name:String; Level:int; PetQuality:string; Power:int; Speed:int; IsFavorite:bool; Creature:string; CanBattle:bool}
    type Quality = 
        |Poor
        |Common
        |Uncommon
        |Rare
        |Legendary
        
    type CharacterInfo = {LastModified: System.Int64; Name:string; Realm:string; Battlegroup:string; Class:int; Race:int; Gender:bool; Level:int; AchievementPoints:int; PetsCollected:int; PetsNotCollected:int}
    module JsonWowDeserialization = 
        open Newtonsoft.Json
        type PetStats = {Level:int; PetQualityId:int; Power:int; Speed:int;}
        type PetInfo = {Name:string; Stats:PetStats; IsFavorite:bool; CreatureName:string; CanBattle:bool; QualityId:int }
        type PetSummary= { NumCollected:int; NotCollected:int; Collected: PetInfo list}
        type CharacterRequestInfo = { LastModified: System.Int64; Name:string; Realm:string; Battlegroup:string; Class:int; Race:int; Gender:bool; Level:int; AchievementPoints:int; Pets: PetSummary}
        let mapCharacterRequestInfo (x:CharacterRequestInfo) = 
            let mapQuality = 
                function
                | 0 -> Poor
                | 1 -> Common
                | 2 -> Uncommon
                | 3 -> Rare
                | 4 -> Legendary
                | x -> failwithf "Unexpected quality: %i" x
                >> sprintf "%A" 
                
            let petDisplays = 
                x.Pets.Collected 
                |> List.map (fun pi -> 
                                    if pi.QualityId <> pi.Stats.PetQualityId then
                                        doLaters.Add(fun () ->
                                            pi.Dump("quality <> Stats.PetQualityId")
                                        )
                                    {
                                        Name=pi.Name
                                        IsFavorite= pi.IsFavorite
                                        Creature=pi.CreatureName
                                        CanBattle=pi.CanBattle
                                        Level=pi.Stats.Level
                                        PetQuality=mapQuality pi.Stats.PetQualityId
                                        Power=pi.Stats.Power
                                        Speed=pi.Stats.Speed})
            {
                CharacterInfo.LastModified = x.LastModified
                Name= x.Name
                Realm= x.Realm
                Battlegroup= x.Battlegroup
                Class= x.Class
                Race = x.Race
                Gender=x.Gender
                Level = x.Level
                AchievementPoints= x.AchievementPoints
                PetsCollected = x.Pets.NumCollected;
                PetsNotCollected = x.Pets.NotCollected;
            }, petDisplays
        
        let deserializeCri s = 
            JsonConvert.DeserializeObject<CharacterRequestInfo>(s) 
            |> mapCharacterRequestInfo
            
    open BattleNet
    
    
    let getPets (key:string) (realm:string) (characterName:string) (fields:string) (locale:string) (jsonp:string) =
        let url = sprintf "%swow/character/%s/%s?fields=pets&locale=%s&apikey=%s" BattleNet.url realm characterName locale key
        url
        |> HttpClient.tryGetUrl List.empty
open BattleNet        
open Warcraft

    
let rawData = Util.Cache(Func<_>(fun _ -> Warcraft.getPets (getSavedApiKey()) "rexxar" "mmsheep" "pets" "en-US" null))
rawData
|> Option.map JsonWowDeserialization.deserializeCri
|> Option.iter (Dump >> ignore)

rawData
|> Option.map prettifyJson
|> Dump
|> ignore
doLaters
|> Seq.iter(fun f-> f())