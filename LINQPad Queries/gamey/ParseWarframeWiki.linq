<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

// pull in data on warframes and items for tracking
open HtmlAgilityPack
open System.Net
let inline getAttr name (node : HtmlNode) = node.GetAttributeValue(name, "")
let getInnerHtml (node : HtmlNode) = node.InnerHtml
let getOuterHtml (node : HtmlNode) = node.OuterHtml
let getName (node : HtmlNode) = node.Name
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
let findChildWithHtmlText (value:string) x = 
    try
        x |> Seq.find(containsHtmlText value)
    with _ ->
        (x |> Seq.head |> fun x -> x.ParentNode.InnerHtml).Dump("failing to find: " + value)
        reraise()
        
type PrimePart = {Name:string; Value:string}

let getWarframe (name:string) url = 
    if name.Contains("Prime") then
        let startingPoint = 
            Util.Cache((fun () -> fetch (sprintf "%s%s" site url)),url)
            |> parse
        try
            startingPoint
            |> selectNodes "//table/tr/td/div" // /tr/td/div/div/div/p/table
            |> findChildWithHtmlText "Blueprint"
            |> selectNodes "div"
            |> findChildWithHtmlText "Blueprint"
            |> selectNodes ".//tr"
            |> Seq.distinct
            |> Seq.map (selectNodes "td")
            |> Seq.collect (Seq.map getInnerHtml)
            |> Seq.distinct
            |> Seq.chunkBySize 2
            |> Seq.map (List.ofSeq >> (function | [part;text] -> {Name=part;Value=Regex.Match(text,"\w+ \w+ \w+").Value} | x -> failwithf "Bad part %A" x))
            |> List.ofSeq
        with _ ->
            (url,Util.OnDemand("raw html", fun () -> startingPoint |> getInnerHtml)).Dump("failing")
            List.empty
    else List.empty

type Warframe = {Name:string; Owned:bool; Parts: PrimePart list; Mastered : bool}
let warframes = 
    Util.Cache((fun () -> fetch targetUrl),"WfComparison")
    |> parse
    |> selectNodes "//table/tr"
    |> Seq.skip 1
    |> Seq.filter(fun x -> not <| isNull x.FirstChild && x.FirstChild.Name <> "th")
    |> Seq.choose (getChildren >> Seq.filter(fun x -> x.Name = "td") >> Seq.tryHead)
    |> Seq.choose (getChildren >> Seq.tryFind(fun x -> x.Name = "a"))
    |> Seq.takeWhile(fun x -> getInnerHtml x |> fun x -> x.[0] <> '<')
    |> Seq.map (fun x -> {Name=x.InnerText; Owned=false; Mastered=false; Parts = getAttr "href" x |> getWarframe x.InnerText})
    |> List.ofSeq
warframes.Dump()

let targetDir =
    let p = Path.GetDirectoryName(Util.CurrentQueryPath)
    let t = Path.Combine(p, "data")
    if not <| Directory.Exists t then
        Directory.CreateDirectory t |> ignore
    t
let targetFile = 
    Path.Combine(targetDir, "warframes.json")
File.WriteAllText(targetFile,Newtonsoft.Json.JsonConvert.SerializeObject(warframes))

printfn "Wrote warframes to %s" targetFile