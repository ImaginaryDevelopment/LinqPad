<Query Kind="FSharpProgram">
  <Reference Relative="..\..\..\PathOfSupporting\PoS\lib\net462\PathOfSupporting.dll">C:\projects\PathOfSupporting\PoS\lib\net462\PathOfSupporting.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>FSharp.Data</NuGetReference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <Namespace>FSharp.Data</Namespace>
  <Namespace>PathOfSupporting</Namespace>
</Query>

// scrape poedb.tw
let waitForAttach = false
open System.Net.Http
open HtmlAgilityPack
open PathOfSupporting.Parsing.Html
open PathOfSupporting.Parsing.Html.PoeDb.Tw
open PathOfSupporting.Parsing.Html.PoeDb.Tw.Impl.Munge
open PathOfSupporting.Parsing.Trees.Gems


open Helpers


module RePoE =
    // Get all possible mods for all item types
    module Option =
        let ofTrue v b = if b then Some v else None
    //type Simple = JsonProvider<""" { "name":"John", "age":94 } """>
    //let simple = Simple.Parse(""" { "name":"Tomas", "age":4 } """)
    [<Literal>]
    let fp = @"C:\projects\poeaffix.github.io\mods.json" // from https://github.com/brather1ng/RePoE/blob/master/data/mods.json
    // somewhat of a map at https://github.com/PoESkillTree/PoESkillTree/blob/master/PoESkillTree.GameModel/Items/ItemClass.cs#L64
    //type ModJp = JsonProvider<ResolutionFolder= @"C:\projects\poeaffix.github.io", Sample="mods.json">
    //type ModJp = JsonProvider<Sample=fp>
    //let modJp = ModJp.Load fp
    [<NoComparison>]
    type MappedMod = {Name:string;Spawn_Weights:(string*int) seq;Raw:JsonValue}
    let toMapped name (x:JsonValue) = 
        {Name=name;Raw=x;Spawn_Weights = x.GetProperty "spawn_weights" |> fun x -> x.AsArray() |> Seq.map(fun x -> x.GetProperty("tag").AsString(),x.GetProperty("weight").AsInteger())}
    [<NoComparison>]
    type GenerationTypeGroup = {GenerationType:string;Items :MappedMod seq}
    [<NoComparison>]
    type DomainContainer={Domain:string;GtGroups: GenerationTypeGroup seq}
    let (|IsRing|_|) ({Spawn_Weights=x} as item) =
        x
        |> Seq.exists(fst >> (=) "ring")
        |> Option.ofTrue item
    let domainGtGroupsLens f x = {x with GtGroups= f x.GtGroups}
    let gtItemsLens f x = {x with Items=f x.Items}
    let domainItemsLens f x = domainGtGroupsLens (Seq.map (gtItemsLens f)) x
    let isRingOrUnique gt =
        function
        | IsRing x -> Some x
        | x -> if gt = "unique" then Some x else None
    let chooseItems f x =
        x
        |> Seq.map(fun d -> {d with GtGroups=d.GtGroups |> Seq.map(fun gtg -> {gtg with Items= gtg.Items |> Seq.choose (f d.Domain gtg.GenerationType) })})
        
    
    let info = lazy(JsonValue.Load fp)
    let getData () =
        info.Value.Properties()
        |> Seq.groupBy(snd >> fun x -> x.GetProperty "domain" |>fun p -> p.AsString())
        |> Seq.map(fun (x,items) -> x,items |> Seq.groupBy(snd >> fun x -> x.GetProperty "generation_type"))
        |> Seq.map(fun (x,gtGroups) ->
            {   Domain=x
                GtGroups=   gtGroups
                            |> Seq.map(fun (gt,items) -> {GenerationType=gt.AsString();Items=items|> Seq.map (fun (name,v) -> toMapped name v )})})

open RePoE
open PathOfSupporting.Generating.PoEAffix
open PathOfSupporting.Generating.Impl.FsHtml

let generateMod {Spawn_Weights=sw;Raw=jv} =
    ul [] [
        yield li ["data-mod-prop"%="spawn_weights"] [
            Text ("Spawn Weights")
            ul [] [
                for (w,v) in sw do
                    yield li [] %(sprintf "%s:%i" w v)
            ]
        ]
        for (name,p) in jv.Properties() do
            yield li [] %(sprintf "%s:%A" name p)
    ]
let generateModGroup {GenerationType=gt;Items =items }= 
    div ["data-generation-type"%=gt] [
        ul [] [
            for item in items do
                yield li ["data-item"%=item.Name] [
                    yield! Impl.generateToggle item.Name []  [Text item.Name] [] [generateMod item]
            ]
        ]
    ]

[<NoComparison>]
type ContentType =
    |SubBucket of GenerationTypeGroup list
    |Elements of Element list
[<NoComparison>]
type Bucket = {PageName:string;Left:ContentType; Right:ContentType;CenterTrail:ContentType}
// generate a single page
let generateRePoEPage {PageName=pgName;Left=left;Right=right;CenterTrail=ct} = 
    printfn "Generating %s" pgName
    let html =
        let processCT =
            function
            |Elements el -> el
            |SubBucket gtg -> gtg |> List.map generateModGroup
        AffixPages.generateAffixPage (sprintf "RePoE %s" pgName) 1
            {   Main=[h2[] []] // %("Prefix")]
                Main2=[h2[] %(pgName)]
                Main3=[h2[] []] // %("Suffix")]
                Corruption= [] // corruption
                InsertFullNav=true
                EnchantPage=None
                Left=processCT left
                Right=processCT right
                TrailingCenter=processCT ct
                Updated=DateTime.UtcNow
            }
        |> PathOfSupporting.Generating.PoEAffix.Impl.guardIds "fin"
        |> toString
        |> sprintf "<!doctype html>\r\n%s"
    let path = sprintf @"c:\projects\poeaffix.github.io\RePoE\%s.html" pgName
    path.Dump("generating into")
    if path |> Path.GetDirectoryName |> Directory.Exists |> not then
        path |> Path.GetDirectoryName |> Directory.CreateDirectory |> ignore
    File.WriteAllText(path,contents=html)
    
// generate all the pages
let parseRePoEMods () =
    RePoE.getData()
    |> Seq.iter(fun x ->
    // item affixes are huge, break them up into chunks
    if x.Domain ="item" then
        let all = x.GtGroups |> List.ofSeq
        
        // generate conglomerate
        generateRePoEPage {PageName=x.Domain;Left=SubBucket all;Right=SubBucket List.empty;CenterTrail=Elements List.empty}
        
        
        // also buckets: prefix/suffix
        // implicits
        x.GtGroups
        |> Seq.groupBy(fun gtg ->
            gtg.GenerationType
        )
        |> Seq.iter(fun (gt,xs) ->
            let page = sprintf "%s-%s" x.Domain gt
            generateRePoEPage {PageName=page;Left=xs |> List.ofSeq |> SubBucket;Right=Elements List.empty;CenterTrail=Elements List.empty}
        )
    else generateRePoEPage {PageName=x.Domain;Left=x.GtGroups|> List.ofSeq |> SubBucket ;Right=Elements List.empty;CenterTrail=Elements List.empty}
)
//PathOfSupporting.Generating.PoEAffix.Impl.generateIndex DateTime.UtcNow
parseRePoEMods()
PathOfSupporting.Generating.PoEAffix.Impl.scrapeAll DateTime.UtcNow