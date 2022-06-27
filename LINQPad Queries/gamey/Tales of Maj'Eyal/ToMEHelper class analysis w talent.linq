<Query Kind="FSharpProgram">
  <Reference>D:\projects\ToMEHelper\src\ToMEHelper\bin\Debug\net5.0\ToMEHelper.dll</Reference>
  <Namespace>System.Net.Http</Namespace>
  <Namespace>ToMEHelper.BHelpers</Namespace>
  <Namespace>ToMEHelper.Schema</Namespace>
  <Namespace>ToMEHelper.Scraping.ApiHelpers</Namespace>
</Query>

// limit to a specific class, characters that did buy a certain talent or category
let baseUrl = Uri "http://zigur.te4.org"
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
let fUnprocessedCount, gUnCount = makeDCCount "unreadable count"
let fProcessedCount, gProCount = makeDCCount "char processed count"
let fLastUrl = makeDC "last url" lastUrl
let fNoDie, _ = makeDCCount "characters with nodeaths"
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
let pageLimit = Some 8
let parallelism = 2
type ClassAnalysis = {
    Category: string
    Mastery: float
    Talents: (string * float) list
}
type CharSummary = {
    Name:string
    Game:string
    Difficulty: string
    Permadeath: string
    Addons: Map<string,string>
} 

ToMEHelper.Scraping.ApiSamples.findAllCharsAsync None None hc {
    Permadeath = Some Permadeath.Roguelike
    Difficulty = None
    Winner = Some true
    Race = Some ToMERace.Ogre
    Alive = None
    Class = Some ToMEClass.SunPaladin
    Campaign = Some Campaign.Maj
    LevelMin = None // Some 10
    LevelMax = None
    Versions = List.empty // [ "tome-1.7.2" ] // "tome-1.6.7" ]
    OnlyOfficialAddons = None // Some true
    ProfileId = None // Some 31919
} 
|> Seq.map( fun x ->
    async {
        match! x with
        | Ok v -> 
            fPullCount v.Length
            
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
//|> Seq.skip 1
//|> fetch1ValidCharDiag
|> Seq.choose(
    function
    | Ok x ->
        fProcessedCount 1
        if x.Talents |> Seq.exists(fun t -> t.Key = "Spell / Staff combat") then
            let sum = {
                Name= x.Character.name
                Game= x.Character.game
                Difficulty= x.Character.difficulty
                Permadeath= x.Character.permadeath
                Addons= x.Character.addons |> Seq.map (|KeyValue|) |> Map.ofSeq
            }
            Some (sum,x.Quests, x.Talents
            
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
            |> List.ofSeq)
        else None
    | Error e -> e.Dump("failing"); failwith "bad fetch"
)
|> Seq.map(fun (a,q,c) -> a, q, c |> List.filter(fun x -> x.Category |> hasHigh |> not))
|> List.ofSeq
|> fun chars ->
    procsw.Stop()
    pullsw.Stop()
    fProc <| gProCount()
    fPull <| gPullCount()
    // category -> characters with category count
    let didinvest = (Map.empty, chars) ||> Seq.fold(fun m (_,_,b) ->
        (m,b)
        ||> Seq.fold(fun m ca ->
            match m |> Map.tryFind ca.Category with
            | Some count -> m |> Map.add ca.Category (count + 1)
            | None -> m |> Map.add ca.Category 1
        )
    )
    let getAvgChars (count:int) = float count / float chars.Length * 100. |> sprintf "%.1f"
    // count of how many characters invested in each category
    let catCount =
        chars
        |> Seq.colect(fun (_,_,c) -> c)
        |> Seq.map(fun (n, ct) ->
            sprintf "%40s %3i %s" n ct (getAvgChars ct)
        )
        |> String.concat "\r\n"
    catCount.Dump("category invest")
    didinvest |> Map.map(fun c count ->
        {|
            // how many chars invested
            Count = count
            // pct of chars that did invest
            PctInvested = getAvgChars count
            // average of players that did invest in the category, how many points did they spend>
            PtAverage =
                chars
                |> Seq.choose(fun (_,_,cas) ->
                    cas
                    |> List.tryFind(fun x -> x.Category = c)
                    |> Option.map(fun ca -> ca.Talents |> List.sumBy snd)
                )
                |> Seq.average
            DepthAverage =
                chars
                |> Seq.choose (fun (_,_,cas) ->
                    cas
                    |> List.tryFind(fun x -> x.Category = c)
                    |> Option.map(fun ca -> ca.Talents |> Seq.length |> float)
                )
                |> Seq.average
        |}
    )
    |> fun x ->
        Map.toSeq x
        |> Seq.sortByDescending(fun (_,x) -> x.Count, x.PtAverage)
        |> fun x -> x.Dump("did invest")
    // run talent invest analysis (of those that went into the category, how many points were spent in each talent
    //chars
    //|> Seq.collect(fun (_,_,talents) ->
    //    let cTalents =
    //        talents
    //        |> Seq.map(fun t ->
    //            t.Category, t.Talents
    //        )
    //        |> Map.ofSeq
    //    
    //)
    //|> fun x -> x
    chars
    |> Dump
    |> ignore
