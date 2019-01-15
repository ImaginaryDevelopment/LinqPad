<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
</Query>

// scrape poedb.tw
open System.Net.Http
open HtmlAgilityPack

module Helpers =
    let delimit d items = String.Join(d,value=Array.ofSeq items)
    let valueStringMap f = function | null | "" as x -> x | x -> f x
    let trim = valueStringMap (fun x -> x.Trim())
    let afterOrSelf d = valueStringMap(fun x ->
        let i = x.IndexOf(d:string)
        if i < 0 then x
        else x.[i+d.Length..]
        )
open Helpers
    
module Seq =
    let tryTail items =
        items
        |> Seq.mapi(fun i x -> if i = 0 then None else Some x)
        |> Seq.choose id
        
[<NoComparison>]
type Wrap = private {node:HtmlNode}
    with
//            static member Wrap n = {node=n}
        member x.Value:HtmlNode option= Option.ofObj x.node
        static member internal getValue (w:Wrap):HtmlNode option = w.Value
        member x.ToDump() = x.Value |> Option.map(fun x -> x.OuterHtml)
module Html =
    let wrap x = {node=x}
    let wrapOpt = Option.defaultValue {node=null}
    let map f =
        Wrap.getValue
        >> Option.map f
    let mapNode f =
        Wrap.getValue
        >> Option.map (f>>wrap)
        >> Option.defaultValue {node=null}
    let mapNull f =
        Wrap.getValue
        >> Option.map f
        >> Option.defaultValue null
    let bind f =
        Wrap.getValue
        >> Option.bind f
    
    let getAttr name =
        Wrap.getValue
        >> Option.bind(fun w -> w.Attributes |> Seq.tryFind(fun xa -> xa.Name = name))
    let getAttrValue name =
        getAttr name
        >> Option.map(fun x -> x.Value)
    let getAttrValueOrNull name =
        getAttrValue name
        >> Option.defaultValue null
    let getInnerText = Wrap.getValue >> Option.map (fun x -> x.InnerText)
    let getFirstChild = mapNode (fun x -> x.FirstChild)
    let getOuterHtml = mapNull(fun x -> x.OuterHtml)
    let getParentNode = mapNode(fun x -> x.ParentNode)
    let selectNodes (xpath:string)= bind(fun x -> x.SelectNodes(xpath) |> Option.ofObj |> Option.map (Seq.map wrap)) >> Option.defaultValue Seq.empty
    let selectNode (xpath:string) = mapNode(fun x -> x.SelectSingleNode xpath)
    let getChildNodes = map(fun x -> x.ChildNodes |> Seq.cast<HtmlNode> |> Seq.map wrap) >> Option.defaultValue Seq.empty
    let getNextSibling = map(fun x -> x.NextSibling |> wrap)
    let getNodeType nt = map(fun x -> x.NodeType = nt) >> Option.defaultValue false
    let collectHtml x = x |> Seq.map getOuterHtml |> delimit String.Empty
    let getHasClass cls = map(fun x -> x.HasClass cls) >> Option.defaultValue false
    let getFollowingSiblings = 
        Seq.unfold(getNextSibling >>
                function
                | None -> None
                | Some sibling -> Some(sibling,sibling)
            )
    let hasText = map(fun x -> x.InnerText |> String.IsNullOrWhiteSpace |> not) >> Option.defaultValue false

open Html
        
let fetch (target:string) =
    async{
        use hc = new HttpClient()
        return! Async.AwaitTask(hc.GetStringAsync target)
        }
let cache key f =
    Util.Cache(Func<_>(f),key=key)
let cacheFetch key =
    cache key (fun () ->
        fetch key
        |> Async.RunSynchronously
    )
    
let parse x =
    let hd = HtmlDocument()
    hd.LoadHtml x
    hd
type TieredAffix={Range:string;Tier:string;Source:string}
type MungedAffix={Display:string;FossilMods:string list;Tiers:TieredAffix list}
let mungeSubCatPair (subCatNode,detailNode) =
    let subId = subCatNode |> getAttrValueOrNull "id" |> afterOrSelf "accordion"
    let detailId = detailNode|> getAttrValueOrNull "id" |> afterOrSelf "collapseOne"
    if String.IsNullOrWhiteSpace subId || subId <> detailId then
        (subId,detailId,subCatNode,detailNode).Dump("fail")
        failwith "failing failed frotteurism"
    let fossilCategories =
            subCatNode |> selectNodes(".//*[contains(@class,'badge')]") |> Seq.choose getInnerText |> List.ofSeq
    let affixName = subCatNode|> getChildNodes |> Seq.skipWhile(hasText >> not) |> Seq.tryTail |> collectHtml
    let detailTable = detailNode |> selectNode ".//table"
    let detailHead = detailTable |> selectNodes ".//thead/tr" |> Seq.tryHead |> wrapOpt
    // the tbody is coming back as not directly in the table :sad face:
    let detailBody = detailTable |> selectNodes ".//tbody/tr"
    affixName,fossilCategories, detailHead,detailBody, detailNode
let mungeAffix titleNode =
    match titleNode |> selectNodes "h4" |> Seq.tryHead |> Option.bind getInnerText with
    | None -> None
    // an affix or suffix
    | Some effixType ->
        let subCategories =
            titleNode
            |> getChildNodes
            |> Seq.skipWhile (getNodeType HtmlNodeType.Element>>not)
            |> Seq.tryTail
            |> Seq.filter (getInnerText >> Option.defaultValue null >>String.IsNullOrWhiteSpace>>not)
            |> Seq.pairwise
            |> Seq.filter(fst>>getHasClass "mod-title")
            |> Seq.map mungeSubCatPair
        // title node has (dummy whitespace text node, x badge nodes, marked up text
//        (effixType,subCategories).Dump("tryTail")
         
        Some(trim effixType,subCategories)
        
// repair the elder and shaper things not having a prefix/suffix attached to the names
let fixUpEffixCategories x =
        x
        |> Seq.pairwise
        |> Seq.collect(fun ((prevTitle,x),(title,y)) ->
            if prevTitle = title && not <| String.IsNullOrWhiteSpace title then
                [(sprintf "%s Prefix" prevTitle,x);(sprintf "%s Suffix" title,y)]
            else [prevTitle,x;title,y]
        )
        |> Seq.distinctBy fst
        |> Seq.filter(fst>>fun x -> x.Contains("suffix",StringComparison.InvariantCultureIgnoreCase) || x.Contains("prefix", StringComparison.InvariantCultureIgnoreCase))
// headerNode is (H4 Amulets... but should be Prefix;Suffix;...)
let mungeModCategory headerNode =
    headerNode.Dump("header")
    let category = getInnerText headerNode |> Option.defaultValue null |> trim
    category , 
        getFollowingSiblings headerNode
        |> Seq.choose mungeAffix
        
let (>&>) f1 f2 x = f1 x && f2 x

let munge (hd:HtmlDocument) =
    hd.DocumentNode
    |> wrap
    |> selectNodes("//div/h4")
    |> Seq.filter(fun x ->
            x |> getAttrValueOrNull "id" |> isNull
            && getChildNodes x |> Seq.exists(Wrap.getValue>> Option.isSome >&> getNodeType HtmlNodeType.Text>>not))
    
    |> Seq.map (mungeModCategory>>(fun (x,y) -> x, fixUpEffixCategories y))
    |> Seq.truncate 2
//    |> Seq.map(fun (x,y) -> x, y |> Seq.truncate 2)
    
cacheFetch "http://poedb.tw/us/mod.php?cn=Amulet"
|> parse
|> munge
|> Seq.collect snd
|> Dump
|> ignore