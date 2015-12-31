<Query Kind="FSharpProgram">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>HtmlAgilityPack</Namespace>
</Query>

// scrape calls for service
let target = @"http://callsforservice.jaxsheriff.org/"

let html = 
    Util.Cache( fun () -> 
        use hc = System.Net.Http.HttpClient()
        hc.GetStringAsync(target)
        |> fun t -> t.Result
    )

let htmlDoc = new HtmlDocument()
htmlDoc.LoadHtml(html)
if not <| isNull htmlDoc.ParseErrors && htmlDoc.ParseErrors.Any() then
    htmlDoc.ParseErrors.Dump()
else
    let body = htmlDoc.DocumentNode.SelectSingleNode("//body")
    let table = body.SelectSingleNode("//div[@id='ctl00_CPH_updCallsForService']//table")
    //table.OuterHtml.Dump()
    table.SelectNodes("./tbody/tr")
    |> Seq.cast<HtmlNode>
    |> Seq.map(fun tr -> tr.InnerHtml)
    |> printfn "%A"
    
    //body.InnerHtml.Dump()
//    let table = body.SelectNodes)
//    table.Dump()