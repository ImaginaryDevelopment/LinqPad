<Query Kind="FSharpProgram">
  <Reference>D:\projects\ToMEHelper\src\ToMEHelper\bin\Debug\net5.0\ToMEHelper.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
  <Namespace>System.Net.Http</Namespace>
  <Namespace>ToMEHelper.BHelpers</Namespace>
  <Namespace>ToMEHelper.Schema</Namespace>
  <Namespace>ToMEHelper.Scraping.ApiHelpers</Namespace>
  <Namespace>ToMEHelper.Scraping.ApiSamples</Namespace>
  <Namespace>ToMEHelper.Scraping.ApiSamples.AnalysisSchema</Namespace>
  <Namespace>ToMEHelper.Scraping.Characters</Namespace>
</Query>

// limit to a specific class, characters that did buy a certain talent or category
let baseUrl = Uri "http://zigur.te4.org"

type FilterType =
    | AnyOf
    | AllOf
let requiredCats = AllOf, Set [
]
let requiredTalents = AllOf, Set [

]
let outpath = 
    let cqp = Util.CurrentQueryPath
    let tomeLPath = Path.GetFullPath cqp |> Path.GetDirectoryName
    let gameyPath =  tomeLPath |> Path.GetDirectoryName
    let tomeDataPath = Path.Combine(gameyPath, "Data", tomeLPath |> Path.GetFileName)
    if not <| Directory.Exists tomeDataPath then
        Directory.CreateDirectory tomeDataPath |> ignore
    let p = Path.Combine(tomeDataPath, Path.GetFileNameWithoutExtension cqp |> sprintf "%s.json")
    p

let mutable lastUrl = None

let hasHigh (v:string) = v |> Seq.exists(fun c -> int c > 260)
    
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
    let s =
        fun v ->
            backing <- v + backing
            f (Some backing)
    (s, fun () -> backing)
        
let makeRateDC title =
    let sw = Stopwatch()
    let fDisplay: string option -> unit = makeDC (sprintf "%s rate" title) None
    let fUpdate x =
        let rate = (float x) / sw.Elapsed.TotalSeconds
        fDisplay (sprintf "%f per second" rate |> Some)
    fUpdate,sw
    
let fPullCount, gPullCount = makeDCCount "char pull count"
let fPageCount, gPageCount = makeDCCount "pg count"
let fUnprocessedCount, gUnCount = makeDCCount "unreadable count"
let fProcessedCount, gProCount = makeDCCount "char processed count"
let fLastUrl = makeDC "last url" lastUrl
let fPull, pullsw = makeRateDC "pull"
let fProc, procsw = makeRateDC "proc"
    
let apiPair =
    let apiId = int <| Util.GetPassword("te4apiId")
    let apiKey = Util.GetPassword("te4apiKey") |> Guid.Parse 
    {
        Id= apiId
        Key= apiKey
    }
    
open ToMEHelper.BHelpers.StringHelpers
module Cereal =
    let serialize (x:'t) =
        //let opts = System.Text.Json.JsonSerializerOptions()
        //System.Text.Json.JsonSerializer.Serialize(x, opts)
        Newtonsoft.Json.JsonConvert.SerializeObject(x)
        
    let inline deserialize<'t>(x:string) =
        Newtonsoft.Json.JsonConvert.DeserializeObject<'t>(x)
        
        //System.Text.Json.JsonSerializer.Deserialize<'t>(x,jsonOpts)
    let jsonOpts = System.Text.Json.JsonSerializerOptions(PropertyNameCaseInsensitive = true)
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

let runTryValidate x =
    x
    |> ApiResultRaw.ToApiResult 
    |> ApiResult.TryValidate
    |> function
        | Some x -> Some x
        | None -> fUnprocessedCount 1; None
let validateAndFetch x =
    match runTryValidate x with
    | None -> None
    | Some v ->
        async {
            let! c = fetchValidChar v |> Async.AwaitTask
            return ToMEHelper.Scraping.ApiSamples.tryDeserializeApiCharacter tomeLogger c
        }
        |> Some
pullsw.Start()
procsw.Start()
let pageLimit = Some 14
let parallelism = 2

let filter = {
    Permadeath = Some Permadeath.Roguelike
    Difficulty = None
    Winner = Some true
    Race = None // Some ToMERace.Ogre
    Alive = None
    Class = None // Some ToMEClass.SunPaladin
    Campaign = Some Campaign.Maj
    LevelMin = None // Some 10
    LevelMax = None
    Versions = [ "tome-1.7.2"; "tome-1.7.3"; "tome-1.7.4" ] // "tome-1.6.7" ]
    OnlyOfficialAddons = Some true
    ProfileId = None // Some 31919
}
        
let requireCats =
    matchReqT requiredCats (fun x -> x.Talents |> Seq.map (|KeyValue|) |> Seq.map fst)
let requireTalents =
    matchReqT requiredTalents (fun x -> x.Talents |> Seq.map (|KeyValue|) |> Seq.map snd |> Seq.collect(fun x -> x.List) |> Seq.map(fun t -> t.Name))
    
ToMEHelper.Scraping.ApiSamples.findAllCharsAsync None None hc filter
|> Seq.map( fun x ->
    async {
        match! x with
        | Ok v -> 
            fPullCount v.Length
            fPageCount 1
            
            return Ok v
        | Error e -> e.Dump("failing"); return Error e
    }
)
|> fun x ->
    match pageLimit with
    | Some l when l > 0 ->
        x
        |> Seq.truncate l
    | None -> x
    | Some v -> failwithf "%i is bad value for page limit" v
|> Seq.chunkBySize parallelism
|> Seq.map Async.Parallel
|> Seq.map(fun a ->
    async {
        let! chunks = a
        let result = chunks |> Seq.choose(function |Ok x -> Some x | Error _ -> None) |> Seq.collect id |> Seq.choose(validateAndFetch) |> Seq.chunkBySize parallelism |> Seq.map Async.Parallel
        return result
    }
)
|> fun x -> x
|> Seq.collect Async.RunSynchronously
|> fun x -> x
|> Seq.map (Async.RunSynchronously)
|> fun x -> x
|> Seq.collect id
|> Seq.map (fun x ->
    fProcessedCount 1
    match x with
    | Ok x -> x
    | Error e -> e.Dump("failing"); failwith "bad fetch"
)
|> Seq.choose requireCats
|> Seq.choose requireTalents
//|> Seq.skip 1
//|> fetch1ValidCharDiag
|> Seq.map(fun x ->
    let sum = {
        Name= x.Character.name
        Game= x.Character.game
        Difficulty= x.Character.difficulty
        Permadeath= x.Character.permadeath
        Addons= x.Character.addons |> Seq.map (|KeyValue|) |> Map.ofSeq
    }
    sum,x.Quests, x.Talents
    
    |> Seq.map (|KeyValue|)
    |> Seq.map(fun (k,v) ->
        k, v.Mastery, v.List |> Array.map(fun tx -> 
                            tx.Name, tx.Val |> before "/" |> float
        )
    )
    |> Seq.choose(fun (k,m, values) ->
        if values |> Seq.exists(snd>>fun x -> x > 0.) then
            Some (k,m,values |> Array.filter(snd >> fun x -> x > 0.))
        else None
    )
    |> Seq.map(fun (k,m,values) ->
        {   Category = k
            Mastery = float m
            Talents = values |> List.ofArray
        }
    )
    |> Seq.sortByDescending(fun x ->
        x.Talents |> Seq.sumBy(snd)
    )
    |> List.ofSeq
)
|> Seq.map(fun (a,q,c) -> a, q, c |> List.filter(fun x -> x.Category |> hasHigh |> not))
|> List.ofSeq
|> fun chars ->
    procsw.Stop()
    pullsw.Stop()
    fProc <| gProCount()
    fPull <| gPullCount()
    let data = {
    
            PageLimit = pageLimit
            Filter = filter
            Chars =
                chars
                |> Seq.map(fun (c,_q,ca) ->
                    c,ca |> Array.ofList
                )
                |> Array.ofSeq
        }
    let cereal = data |> serialize
    cereal |> deserialize<CharAnalysisData> |> ignore
    File.WriteAllText(outpath,cereal)
    printfn "Wrote data to '%s' outpath"
