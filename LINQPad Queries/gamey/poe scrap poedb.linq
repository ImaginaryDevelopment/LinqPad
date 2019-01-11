<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
</Query>

// scrape poedb.tw
open System.Net.Http
open HtmlAgilityPack
    
[<NoComparison>]
type Wrap = private {node:HtmlNode}
    with
//            static member Wrap n = {node=n}
        member x.Value:HtmlNode option= Option.ofObj x.node
        static member internal getValue (w:Wrap):HtmlNode option = w.Value
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
    let selectNodes (xpath:string)= map(fun x -> x.SelectNodes(xpath) |> Seq.map wrap)
    let getChildNodes = map(fun x -> x.ChildNodes |> Seq.cast<HtmlNode> |> Seq.map wrap) >> Option.defaultValue Seq.empty

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
let mungeAffix titleNode =
    let fossilCategory = titleNode |> getFirstChild |> Option.map getOuterHtml |> Option.defaultValue null
    fossilCategory, titleNode |> getOuterHtml
let mungeModCategory headerNode =
    let category = getInnerText headerNode
    category ,
        getParentNode headerNode
        |> Option.bind (selectNodes(".//*[@class='mod-title']"))
        |> Option.map (Seq.map mungeAffix)
    
let munge (hd:HtmlDocument) =
    hd.DocumentNode
    |> wrap
    |> selectNodes("//div/h4")
    |> Option.map(
        Seq.filter(fun x ->
            x |> getAttrValueOrNull "id" |> isNull
            && getChildNodes x |> Seq.exists(fun cn -> cn.NodeType <> HtmlNodeType.Text)) |> not)
    )
    
    |> Seq.map mungeModCategory
    |> Seq.truncate 2
    |> Seq.map(fun (x,y) -> x, y |> Seq.truncate 2)
//    |> Seq.map(fun x -> x.OuterHtml)
    
cacheFetch "http://poedb.tw/us/mod.php?cn=Amulet"
|> parse
|> munge
|> Dump
|> ignore