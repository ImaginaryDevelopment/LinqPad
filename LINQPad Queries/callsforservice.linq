<Query Kind="FSharpProgram">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>HtmlAgilityPack</Namespace>
</Query>

// scrape calls for service
let target = @"http://callsforservice.jaxsheriff.org/"

let html = 
    Util.Cache( fun () -> 
        use hc = new System.Net.Http.HttpClient()
        hc.GetStringAsync(target)
        |> fun t -> t.Result
    )
let selectNode s (node:HtmlNode) = node.SelectSingleNode s
let selectNodes s (node:HtmlNode) = node.SelectNodes s |> Seq.cast<HtmlNode>
let sprintNode (node:HtmlNode) = node.OuterHtml
let htmlDoc = new HtmlDocument()
htmlDoc.LoadHtml(html)

type IncidentRow = { Incident:string; DispatchTime:string; Address:string;AddressLink:string;Description:string;Comments:string}

if not <| isNull htmlDoc.ParseErrors && htmlDoc.ParseErrors.Any() then
    htmlDoc.ParseErrors.Dump()
else
    let body = htmlDoc.DocumentNode.SelectSingleNode("//body")
    let table = body.SelectSingleNode("//div[@id='ctl00_CPH_updCallsForService']//table")
    let mapDispatch node = ()
        
    let mapTr (node:HtmlNode) = 
        let mapRow (items:HtmlNode list) = 
            let mapAddress n = 
                selectNode "./a" n
                |> (fun n -> n.Attributes.["href"].Value, n.InnerText)
            
            match items with
            |[incidentContainer;dispatchTimeContainer;addressContainer;signalContainer;descriptionContainer;commentsContainer] -> 
                let link,address = mapAddress addressContainer
                {
                    Incident = incidentContainer.InnerText
                    DispatchTime = dispatchTimeContainer.InnerText
                    Address= address
                    AddressLink= link
                    Description= descriptionContainer.InnerText
                    Comments=commentsContainer.InnerText
                } //,sprintf "%A" (List.map sprintNode items)
            |_ -> failwithf "unexpected list %A" items
            
        selectNodes "./td" node
        |> List.ofSeq
        |> mapRow
        
        //|> List.ofSeq
        //|> Seq.map (fun (a,b,c) -> sprintNode a, sprintNode b, sprintNode c)
        
    
        
    //table.OuterHtml.Dump()
    table.SelectNodes("./tbody/tr")
    |> Seq.cast<HtmlNode>
    |> Seq.map mapTr
    //|> Seq.map(fun tr -> tr.InnerHtml)
    |> Array.ofSeq
    |> printfn "%A"
    
    //body.InnerHtml.Dump()
//    let table = body.SelectNodes)
//    table.Dump()