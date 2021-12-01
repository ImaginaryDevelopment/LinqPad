<Query Kind="FSharpProgram">
  <Reference Relative="..\..\..\PathOfSupporting\PoS\lib\net462\PathOfSupporting.dll">C:\projects\PathOfSupporting\PoS\lib\net462\PathOfSupporting.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>FSharp.Data</Namespace>
  <Namespace>Newtonsoft.Json</Namespace>
  <Namespace>PathOfSupporting</Namespace>
</Query>

// scrape poedb.tw into json
let inline valueFunc f = function | null | "" as x -> x | x -> f x
let inline replace d r = valueFunc(fun x -> x.Replace(oldValue=d,newValue=r))
let inline remove d = replace d String.Empty
let inline rReplace p r = valueFunc (fun x -> Regex.Replace(x,p,replacement=r))
let inline delimit d items = String.Join(d,value=items)
let toMapList =
    Seq.groupBy fst
    >> Seq.map(fun (k,items) -> k, items |> Seq.map snd |> List.ofSeq)
    >> Map.ofSeq
open PathOfSupporting.Parsing.Html
open PathOfSupporting.Parsing.Html.PoeDb.Tw
open PathOfSupporting.Parsing.Html.PoeDb.Tw.Impl.Munge
open PathOfSupporting.Parsing.Trees.Gems
open HtmlAgilityPack
open PathOfSupporting.Internal.BReusable.StringPatterns

let targets = [
            // poedb name, local file name, corruption element
            "Amulet","ac-amulet"
            "Bow","2h-bow"
            "Belt","ac-belt"
            "Claw","1h-claw"
            "Dagger","1h-dagger"
            "FishingRod","2h-fish"
            "One Hand Axe","1h-axe"
            "One Hand Mace","1h-mace"
            "One Hand Sword","1h-sword"
            "Quiver","ac-quiver"
            "Ring","ac-ring"
            "Sceptre","1h-sceptre"
            "Staff","2h-staff"
            "Two Hand Axe", "2h-axe"
            "Two Hand Mace","2h-mace"
            "Two Hand Sword", "2h-sword"
            "Wand","1h-wand"
        ]
let getValues () =
    let values =
        targets
        |> List.map(fun (cn,_pg) -> {cn=cn;an=null})
        |> List.map (PoeDb.Tw.parseModPhp )
        |> Async.Parallel
        |> Async.RunSynchronously
    let values,errors =
        ((List.empty,List.empty),values)
        ||> Seq.fold(fun (oks,errors) value ->
            match value with
            | Ok x -> x::oks,errors
            | Error x -> oks,x::errors
        )
    match errors with
    | [] -> ()
    | _ ->
        errors
        |> Dump
        |> ignore
        failwithf "Errors detected aborting"
    values
    |> List.collect id
module Helpers =
    let inline store (path,x:'t) =    
        let so = JsonConvert.SerializeObject(x,Formatting.Indented) 
        File.WriteAllText(path,so)
open Helpers
type JReadCache<'t>(value) =
    member __.Value
        with get():'t= value
    new(path) =
        JReadCache(File.ReadAllText path |> JsonConvert.DeserializeObject<'t>)
        
    static member Store path (x:'t) =
        store(path,x)
        JReadCache(x)
    static member ReadOrFetch path (f:unit -> 't):JReadCache<'t> =
        if File.Exists path then
            JReadCache(path=path)
        else
            let values = f()
            JReadCache.Store path values
    
module JReadCache =
    // change the type of a cache?
    let map path f =
        JReadCache(path=path).Value
        |> f
        |> JReadCache.Store path 
        
        
//let cacheValues () =
let cacheDir =    
//    let localAppDataPath = Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData
    let tmp = Path.GetTempPath()
    let cacheDir = Path.Combine(tmp, "LINQPadScriptData",Util.CurrentQuery.Name)
    if not <| Directory.Exists cacheDir then
        Directory.CreateDirectory cacheDir |> ignore
    printfn "Caching to %s" cacheDir
    cacheDir
let valuesPath = Path.Combine(cacheDir,"values.json")
let _f () = JReadCache.map valuesPath (List.collect id)
let getAttr name (w:HtmlNode) = w.Attributes |> Seq.tryFind(fun xa -> xa.Name = name)
let getAttrVal name w = getAttr name w |> Option.map(fun xa -> xa.Value)
let (|SuspectedHtml|_|) =
    function
    | null | "" -> None
    | x when x.Contains("<") || x.Contains(">") -> Some x
    | _ -> None
let (|DataToolTipTag|_|) (x:HtmlNode) =
    match getAttrVal "data-tooltip" x with
    | None -> None
    | Some v -> Some v
    
let (|HasText|EmptyText|HNode|) :HtmlNode -> _ =
    function 
    | :? HtmlTextNode as htn ->
        if  String.IsNullOrWhiteSpace htn.Text |> not then
            HasText htn
        else EmptyText
    | x -> HNode x
    
    
let wrapCreate x =
    let n =
        x
        |> sprintf "<div>%s</div>"
        |> HtmlNode.CreateNode 
        |> fun x -> x.ChildNodes
        |> Seq.toList
    n
// expects a string of spaces and data-tooltip spans only
let getToolTipItems (x:string):(string*string) list = 
    match x with
    | null | "" -> List.empty
    | _ ->
        wrapCreate x
        |> List.choose(
            function
            |HasText cn ->
                (x,cn).Dump("wrapCreate failing")
                failwith "there should not be raw text in here"
            |EmptyText -> None
            |HNode x -> Some x
        )
        |> List.map(
            function
            | DataToolTipTag v as child ->
                v,child.InnerText
            | x ->
                failwithf "uh oh:%s:%A" (x.GetType().Name) x.OuterHtml
        )

let extractTooltips:_ -> string*Map<string,string list> =
    function
    |SuspectedHtml x ->
        let items =
            wrapCreate x
            |> List.map(
                function
                | :? HtmlTextNode as htn -> Choice1Of2 htn.Text
                | DataToolTipTag v as container ->
                    match container.InnerHtml with
                    | ValueString ih ->
                        (container.OuterHtml,v,ih).Dump("failing ovih")
                        failwith "unexpected text"
                    | _ ->
                        getToolTipItems v
                        |> toMapList
                        |> Choice2Of2
                | x -> (x.OuterHtml.Dump("wth getTooltipData")); failwith "bad"
                )
        let initState = (StringBuilder(),Map.empty)
        (initState,items)
        ||> List.fold(fun (sb,m) item ->
            match item with
            | Choice1Of2 txt -> sb.Append txt,m
            |Choice2Of2 m' -> 
                if m.Count > 0 then
                    failwithf "two maps %A" x
                sb,m'
        )
        |> fun (sb,m) -> sb.ToString(), m
    | x -> x,Map.empty
let failHtml name =
    valueFunc(fun x ->
        if x.Contains "<" || x.Contains ">" then
            failwithf "%s:bad cleaning %s" name x
        x
    )
    
let cleanDisplay' =    
        replace "&ndash;" "-"
        >> remove "<br>"
        >> rReplace "<span class='mod-value'>([^<>]+)</span>" "$1"
        
let cleanDisplay name =
    cleanDisplay'
    >> failHtml name
    
let extractDisplay x=
    let f = 
        cleanDisplay'
        >> extractTooltips
    let text,m = f x
    if Regex.IsMatch(text,"&\w+;") then
        (x,text).Dump()
        failwith "escape unescaped"
    if text.Contains "<" || text.Contains ">" then
        (x,text).Dump()
        failwith "bad extraction"
    text,m
let extractMeta x =
    let text = cleanDisplay' x
    
    if not <| isNull text && text.Contains "<" then
        let state = StringBuilder(),None
        (state,wrapCreate text)
        ||> List.fold(fun (sb,essOpt) node ->
            match node with
            | HasText t -> sb.Append t, essOpt
            | EmptyText -> sb,essOpt
            | HNode hn ->
                if hn.Name = "a" then
                    sb, Some hn.InnerText
                else failwithf "what node is this? %A" hn.OuterHtml
        )
        |> fun (sb,essOpt) ->
            sb.ToString(), essOpt |> Option.defaultValue null
    else text,null
    
type TieredAffixStrip = {Tier:string;Meta:string;ILvl:int;Special:Map<string,string list>;Essence:string;Display:string;Chance:string}
let fromTieredAffixContainer (x:TieredAffix) =
    let display,spec = extractDisplay x.DisplayHtml
    let cleanedMeta,ess = extractMeta x.MetaHtml
    {TieredAffixStrip.Tier =x.Tier; Meta=cleanedMeta;ILvl=x.ILvl;Display=display;Special=spec;Chance=x.Chance;Essence=ess}
let affixTierCLens f (x:AffixTierContainer<_>) =
    let display = cleanDisplay "affixTierCLens" x.Display
    {AffixTierContainer.Display = display;FossilCategories=x.FossilCategories;Children=x.Children |> List.map f}
let actualValue =
    JReadCache.ReadOrFetch valuesPath getValues
    |> fun x -> x.Value
let basePath = lazy(
    let bp = "C:\projects\SuperAdvancedFSharpEnigma\src\Client\public\poeaffix"
    if not <| Directory.Exists bp then
        Directory.CreateDirectory bp |> ignore
    bp
    )
actualValue
|> List.map(ItemAffixContainer<_>.mapChildren(AffixContainer.mapChildren(List.map(affixTierCLens fromTieredAffixContainer))))
|> Seq.iter (fun x ->
    let targetPath =
        Path.Combine(basePath.Value, x.ItemType |> replace " " "_" |> sprintf "%s.json")
    store(targetPath,x.Children)
)
//|> Seq.iter ignore
|> Dump
|> ignore