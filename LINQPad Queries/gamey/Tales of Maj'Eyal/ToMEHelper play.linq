<Query Kind="FSharpProgram">
  <Reference>D:\projects\ToMEHelper\src\ToMEHelper\bin\Debug\net5.0\ToMEHelper.dll</Reference>
  <Namespace>System.Net.Http</Namespace>
  <Namespace>ToMEHelper.BHelpers</Namespace>
  <Namespace>ToMEHelper.Schema</Namespace>
  <Namespace>ToMEHelper.Scraping.ApiHelpers</Namespace>
</Query>

let baseUrl = Uri "http://zigur.te4.org"
let apiPair =
    let apiId = int <| Util.GetPassword("te4apiId")
    let apiKey = Util.GetPassword("te4apiKey") |> Guid.Parse 
    {
        Id= apiId
        Key= apiKey
    }
open ToMEHelper.BHelpers.StringHelpers
module Cereal =
    let jsonOpts = System.Text.Json.JsonSerializerOptions(PropertyNameCaseInsensitive = true)
    let inline deserialize<'t>(x:string) =
        System.Text.Json.JsonSerializer.Deserialize<'t>(x,jsonOpts)
    let inline deserializeEle(x:string) =
        System.Text.Json.JsonSerializer.Deserialize<System.Text.Json.JsonElement>(x, jsonOpts)
    let deserializeForPath (propName:string) x =
        let je = deserializeEle x
        je.GetProperty(propName) |> string
        
open Cereal        
    
let hc =
    ToMEHelper.Scraping.ApiSamples.getHC baseUrl (Some (fun x -> x.Dump())) apiPair
    //new HttpClient(BaseAddress= baseUrl)
    
let tomeLogger =
    {   new ToMEHelper.BHelpers.ToMELogger with
            member _.Dump(value) = value |> Dump
            member _.Dump(value,title) = value.Dump(description=title)
    } 
let getFilters() = ToMEHelper.Scraping.ApiSamples.getFilters hc
let getChar charId = ToMEHelper.Scraping.ApiSamples.getChar hc charId
let findChars charId = ToMEHelper.Scraping.ApiSamples.findChars hc charId

let dumpFindCharsValidation extraDebug characterFilter =
    ToMEHelper.Scraping.ApiSamples.dumpFindCharsValidation hc tomeLogger extraDebug characterFilter
        
let fetchChar (x:ApiResult) = hc.GetStringAsync(x.CharsheetApi)
    
let fetchValidChar (x:ValidatedApiResult) = hc.GetStringAsync(x.CharsheetApi)
let fetch1CharDiag chars =
    chars
    |> Seq.head
    |> fetchChar
let fetch1ValidCharDiag chars =
    ToMEHelper.Scraping.ApiSamples.fetch1ValidCharDiag tomeLogger hc chars
// 31919/tome/b5a2c97d-26f0-47a9-9296-23a70cc650d2/json
// works for get char
let myChar = {Owner=OwnerId 31919; Id = Guid.Parse "b5a2c97d-26f0-47a9-9296-23a70cc650d2"}

let otherChar = {Owner=OwnerType.OnlineId 238491; Id = Guid.Parse "631be933-ff8f-4a47-8fb7-84f8d3ea6246"}

//getFilters()
//|> Async.AwaitTask
//|> Async.RunSynchronously
//|> Dump
//|> ignore
ToMEHelper.Scraping.ApiSamples.getProfileIdFromOnlineId hc 31922
|> Dump
|> ignore
findChars {
    Permadeath = None
    Difficulty = None
    Winner = Some true
    Race = None // Some ToMERace.Halfling
    Alive = None
    Class = None // Some ToMEClass.Possessor
    Campaign = None // Some Campaign.Maj
    LevelMin = None // Some 10
    LevelMax = None
    Versions = List.empty
    OnlyOfficialAddons = None // Some true
    ProfileId = Some 31919
} None
|> Async.AwaitTask
|> Async.RunSynchronously
|> deserializeApiResults
|> function
    |Ok x -> x
    | Error e -> e.Dump(); failwith "failing deserializeApiResults"
|> Seq.choose (ApiResultRaw.ToApiResult >> ApiResult.TryValidate)
//|> Seq.skip 1
//|> fetch1ValidCharDiag
|> Seq.map(fun v ->
    fetchValidChar v |> Async.AwaitTask |> Async.RunSynchronously |> ToMEHelper.Scraping.ApiSamples.tryDeserializeApiCharacter tomeLogger
    |> function
        | Ok x -> v,x |> ToMEHelper.Scraping.Characters.RawApiCharacters.removeEmptyTalents
        | Error e -> (v,e).Dump("failing"); failwith "bad fetch"
)
// filter out unreadable localized profiles
|> Seq.filter(fun (_,y) -> y.Character.size |> Seq.exists(fun c -> int c > 1000) |> not)
|> Seq.truncate 12

|> Dump
|> ignore