<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
</Query>

// pull in data on warframes and items for tracking
open HtmlAgilityPack
open System.Net
let inline getAttr name (node : HtmlNode) = 
    node.GetAttributeValue(name, "")
let getInnerHtml (node : HtmlNode) = 
    node.InnerHtml
let getOuterHtml (node : HtmlNode) = 
    node.OuterHtml
let getName (node : HtmlNode) = 
    node.Name
let getChildren (node : HtmlNode) = 
    match node.ChildNodes with
    | null -> []
    | x -> x |> List.ofSeq
let selectNodes selector (node : HtmlNode) = 
    node.SelectNodes(selector)
let site = "http://warframe.wikia.com"
let targetUrl = sprintf "%s/wiki/Warframes_Comparison" site

let fetch (url:string) = 
    printfn "Fetching %s" url
    use wc = new WebClient()
    wc.DownloadString url
    
let parse htmlText = 
    let hd = HtmlDocument()
    hd.LoadHtml htmlText
    hd.DocumentNode
type ItemType = 
    | Item
    | Prime of parts:string list
let containsHtmlText (value:string) (node : HtmlNode) = 
    node.InnerHtml.Contains(value)
let findChildWithHtmlText (value:string) = 
    Seq.find(containsHtmlText value)
    
let getWarframe (name:string) url = 
    if name.Contains("Prime") then
        Util.Cache((fun () -> fetch (sprintf "%s%s" site url)),url)
        |> parse
        |> selectNodes "//table/tr/td/div" // /tr/td/div/div/div/p/table
        |> findChildWithHtmlText "Blueprint"
        |> selectNodes "div"
        |> findChildWithHtmlText "Blueprint"
        |> getInnerHtml
        |> Some
    else None
Util.Cache((fun () -> fetch targetUrl),"WfComparison")
|> parse
|> selectNodes "//table/tr"
|> Seq.skip 1
|> Seq.filter(fun x -> not <| isNull x.FirstChild && x.FirstChild.Name <> "th")
|> Seq.choose (getChildren >> Seq.filter(fun x -> x.Name = "td") >> Seq.tryHead)
|> Seq.choose (getChildren >> Seq.tryFind(fun x -> x.Name = "a"))
|> Seq.takeWhile(fun x -> getInnerHtml x |> fun x -> x.[0] <> '<')
|> Seq.map (fun x -> x.InnerText, getAttr "href" x |> getWarframe x.InnerText)
|> Seq.truncate 2
|> Dump
|> ignore

