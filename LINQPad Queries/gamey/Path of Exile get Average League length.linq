<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <Namespace>System.Net</Namespace>
  <Namespace>System.Net.Http</Namespace>
</Query>

let (|ValueString|NonValueString|) (x:string) = 
    if String.IsNullOrWhiteSpace(x) then
        NonValueString
    else ValueString (x.Trim())

let (|RMatch|_|) p x =
    let m = Regex.Match(x,p)
    if m.Success then
        Some m
    else None
    
let text = 
    let f () =
        use hc = new HttpClient()
        hc.GetStringAsync("https://pathofexile.gamepedia.com/League#Past_leagues").Result
    Util.Cache(f, "html")
let parsed = 
    let doc = new HtmlAgilityPack.HtmlDocument()
    doc.LoadHtml text
    doc
let mdyPattern = "(\w+ \d{1,2},\s*\d{4})"
let leagues =
    let span = parsed.GetElementbyId "Past_leagues"
    span.ParentNode.NextSibling.NextSibling.ChildNodes
    |> Seq.map(fun cn -> cn.InnerText)
    |> Seq.filter(String.IsNullOrWhiteSpace >> not)
    |> Seq.choose ((|RMatch|_|) (sprintf "The (\w+) league .* %s to %s" mdyPattern mdyPattern))
    |> Seq.map(fun m -> m.Groups.[1].Value, DateTime.Parse m.Groups.[2].Value, DateTime.Parse m.Groups.[3].Value)
    |> Seq.map(fun (name,start,end_) -> name, end_ - start,start,end_)
    
leagues
|> Dump
|> Seq.map(fun (_,x,_,_) -> float x.Days)
|> Seq.average
|> Dump
|> ignore
    

