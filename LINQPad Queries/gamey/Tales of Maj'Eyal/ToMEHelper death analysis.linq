<Query Kind="FSharpProgram">
  <Reference>D:\projects\ToMEHelper\src\ToMEHelper\bin\Debug\net5.0\ToMEHelper.dll</Reference>
  <Namespace>System.Net.Http</Namespace>
  <Namespace>ToMEHelper.BHelpers</Namespace>
  <Namespace>ToMEHelper.Schema</Namespace>
  <Namespace>ToMEHelper.Scraping.ApiHelpers</Namespace>
</Query>

let baseUrl = Uri "http://zigur.te4.org"
let mutable lastUrl = None

// option so clearing the dc is possible
let makeDC<'t> (title:string) (initialValue: 't option) =
    let dc = DumpContainer()
    dc.Dump(title)
    let f (v:'t option) =
        match v with
        | None -> dc.Content <- null
        | Some v -> dc.Content <- v
    f initialValue
    f
let makeDCCount title =
    let mutable backing = 0
    let f = makeDC title (Some backing)
    fun v ->
        backing <- v + backing
        f (Some backing)
        
    
let fCount = makeDCCount "char pull count"
let fUnprocessedCount = makeDCCount "unreadable count"
let fProcessedCount = makeDCCount "char processed count"
let fLastUrl = makeDC "last url" lastUrl
let fNoDie = makeDCCount "characters with nodeaths"
let fCeliaKills = makeDCCount "celia raw kills"
let fCeliaUKills = makeDCCount "celia unique kills"

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
let loggingFilter (x:string) =
    if x.EndsWith "/json" then
        ()
    else fLastUrl (Some x)
    
let hc =
    ToMEHelper.Scraping.ApiSamples.getHC baseUrl (Some loggingFilter) apiPair
    
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
//ToMEHelper.Scraping.ApiSamples.getProfileIdFromOnlineId hc 31922
//|> Dump
//|> ignore

ToMEHelper.Scraping.ApiSamples.findAllChars hc {
    Permadeath = None
    Difficulty = None
    Winner = None // Some true
    Race = None // Some ToMERace.Halfling
    Alive = None
    Class = None // Some ToMEClass.Possessor
    Campaign = Some Campaign.Maj
    LevelMin = None // Some 10
    LevelMax = None
    Versions = [ "tome-1.6.7" ]
    OnlyOfficialAddons = None // Some true
    ProfileId = None // Some 31919
} None
|> Seq.truncate (800 / 25)
|> Seq.collect(
    function
    | Ok v ->
        fCount v.Length
        v
    | Error e -> e.Dump("failing"); failwith "found error"
)
|> Seq.choose (fun x ->
    x
    |> ApiResultRaw.ToApiResult 
    |> ApiResult.TryValidate
    |> function
        | Some x -> Some x
        | None -> fUnprocessedCount 1; None
)
//|> Seq.skip 1
//|> fetch1ValidCharDiag
|> Seq.map(fun v ->
    fetchValidChar v |> Async.AwaitTask |> Async.RunSynchronously |> ToMEHelper.Scraping.ApiSamples.tryDeserializeApiCharacter tomeLogger
    |> function
        | Ok x ->
            Async.Sleep(20)
            |> Async.RunSynchronously
            x.Character.game,x.Character.died
        | Error e -> (v,e).Dump("failing"); failwith "bad fetch"
)
|> Seq.choose(fun (_,d) -> 
    fProcessedCount 1
    let msg = "Killed by Celia"
    if d.times > 0. then
        if not <| isNull d.desc then
            if d.desc.Contains msg then
                fCeliaUKills 1
                // faulty for some reason
                let celiakills = d.desc |> split "\r\n" |> Seq.filter (fun x -> x.Contains msg) |> Seq.length
                fCeliaKills celiakills
                Some d.desc
            else None
        else failwith "whut?"
    else 
        fNoDie 1
        None
)
//|> Seq.groupBy(fun (d,g,_) ->
//    g,d.times > 0. && not <| isNull d.desc && d.desc.Contains "Celia"
//)
// filter out unreadable localized profiles
//|> Seq.filter(fun (_,y) -> y.Character.size |> Seq.exists(fun c -> int c > 1000) |> not)
//|> Seq.truncate 20

|> Dump
|> ignore