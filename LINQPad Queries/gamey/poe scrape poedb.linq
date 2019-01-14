<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
</Query>

// scrape poedb.tw
open System.Net.Http
open HtmlAgilityPack

let delimit d items = String.Join(d,value=Array.ofSeq items)
    
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
    let map f =
        Wrap.getValue
        >> Option.map f
    let mapNode f =
        Wrap.getValue
        >> Option.map (f>>wrap)
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
    let selectNodes (xpath:string)= bind(fun x -> x.SelectNodes(xpath) |> Option.ofObj |> Option.map (Seq.map wrap))
    let getChildNodes = map(fun x -> x.ChildNodes |> Seq.cast<HtmlNode> |> Seq.map wrap) >> Option.defaultValue Seq.empty
    let getNextSibling = map(fun x -> x.NextSibling |> wrap)
    let getNodeType nt = map(fun x -> x.NodeType = nt) >> Option.defaultValue false
    let collectHtml x = x |> Seq.map getOuterHtml |> delimit String.Empty

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
type MungedAffix={Display:string;FossilMods:string list}
let mungeAffix titleNode =
    let fossilCategory = 
            titleNode |> selectNodes(".//*[contains(@class,'badge')]") |> Option.defaultValue Seq.empty |> Seq.choose getInnerText |> List.ofSeq
    // title node has (dummy whitespace text node, x badge nodes, marked up text
    {FossilMods=fossilCategory;Display= titleNode |> getChildNodes |> Seq.skipWhile (getNodeType HtmlNodeType.Element>>not) |> Seq.tryTail |> collectHtml} //, titleNode|> getOuterHtml

let mungeModCategory headerNode =
    let category = getInnerText headerNode |> Option.defaultValue null
    category ,
        getParentNode headerNode
        |> Option.bind (selectNodes(".//*[@class='mod-title']"))
        |> Option.defaultValue Seq.empty
        |> Seq.map mungeAffix
let (>&>) f1 f2 x = f1 x && f2 x

let munge (hd:HtmlDocument) =
    hd.DocumentNode
    |> wrap
    |> selectNodes("//div/h4")
    |> Option.map(
        Seq.filter(fun x ->
            x |> getAttrValueOrNull "id" |> isNull
            && getChildNodes x |> Seq.exists(Wrap.getValue>> Option.isSome >&> getNodeType HtmlNodeType.Text>>not))
    
        >> Seq.map mungeModCategory
//    |> Seq.truncate 2
//    |> Seq.map(fun (x,y) -> x, y |> Seq.truncate 2)
    )
    
cacheFetch "http://poedb.tw/us/mod.php?cn=Amulet"
|> parse
|> munge
|> Dump
|> ignore