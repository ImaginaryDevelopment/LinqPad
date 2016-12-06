<Query Kind="FSharpExpression">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <Namespace>HtmlAgilityPack</Namespace>
  <Namespace>System.Net.Http</Namespace>
</Query>

let loadHtml text = 
    let result = HtmlDocument()
    result.LoadHtml text
    result
let dumpc x = (x |> List.length).Dump(); x
let chunkByRowsAfterHead f (xs: 'T list) =
    let len = xs.Length
    let rec loop (xs: 'T list) = 
        [
            let chunk = 
                let headDelimiter = xs.[0]
                let tail = xs |> Seq.skip 1 |> Seq.takeWhile f |> List.ofSeq
                headDelimiter::tail
            yield chunk
            let remainder = xs |> Seq.skip chunk.Length |> List.ofSeq
            if Seq.exists (fun  _ -> true) remainder then
                yield! loop remainder
        ]
    loop xs
                    
let getDescendants (name:string) (he:HtmlNode) = he.Descendants(name)
let getElement name (he:HtmlNode) = he.Element(name)
let getChildren (he:HtmlNode) = he.ChildNodes
let getInnerHtml (he:HtmlNode) = he.InnerHtml
let getOuterHtml (he:HtmlNode) = he.OuterHtml
let dumpOuter (he:HtmlNode) = he.OuterHtml.Dump(); he
let scriptText = 
    Util.Cache((fun () -> 
    // the part I was looking for doesn't seem to be on this site, but it was on this one: https://sfy.ru/?script=batman_production
        use client = new HttpClient(BaseAddress=Uri("http://www.imsdb.com"))
        client.GetStringAsync("/scripts/Batman.html").Result)
        ,"batman")
let isCharacterNameElement (n:HtmlNode) = 
    n.Name="b" &&
    not <| String.IsNullOrWhiteSpace n.InnerText &&
    n.InnerText.Trim() |> Seq.exists (Char.IsUpper >> not) |> not

let getCharacterChunks =
    scriptText
    |> loadHtml
    |> fun x -> x.DocumentNode
    |> getElement "html"
    |> getElement "body"
    |> getDescendants "html"
    |> Seq.head
    |> getElement "body"
    |> getElement "pre"
    //|> dumpOuter
    |> getChildren
    |> Seq.skipWhile(isCharacterNameElement>>not) //fun e -> String.IsNullOrWhiteSpace e.InnerText && e.Name <> "b" && e.InnerText.Trim() |> Seq.exists (Char.IsUpper >> not))
    |> List.ofSeq
    |> chunkByRowsAfterHead (isCharacterNameElement>> not)
    |> Seq.map (List.ofSeq)

    |> List.ofSeq
    
    //|> Seq.map getOuterHtml

getCharacterChunks

//|> Seq.filter(fun chunk -> chunk |> Seq.head |> fun c -> c.InnerText.Trim() = "JOKER")
//|> Seq.map (fun characterNameChunk -> characterNameChunk |> Seq.skip 1 |> List.ofSeq )

|> Seq.map (Seq.map getOuterHtml)
|> Seq.map (Seq.map (fun x -> x.Replace("&","&amp;")))
|> List.ofSeq
|> dumpc
// skip until a certain moment
//|> fun x -> x.Dump(); x
|> Seq.mapi (fun i x -> i,x)
|> Seq.skip 196 // that you sugar bumbs is next ~ 37 min in
|> List.ofSeq
|> dumpc
//|> Seq.skipWhile(snd >> Seq.exists (containsI "mayhem") >> not)
//|> List.ofSeq
//|> dumpc
//|> Seq.skip 1
//|> Seq.skipWhile(fun x -> x |> Seq.exists (contains "JOKER") |> not)
|> Seq.map (fun (i,x) -> i, x |> delimit String.Empty)
|> Seq.map (fun (i,x) -> i,x |> sprintf "<pre>%s</pre>")
|> Seq.map (fun (i,x) -> i, Util.RawHtml x)

    //,Util.RawHtml x)) 
    //|> Seq.map (sprintf "<li>%s</li>") |> delimit String.Empty |> fun x -> x, Util.RawHtml x)
//|> delimit String.Empty
//|> sprintf "<ol>%s</ol>"
//|> Util.RawHtml

//|> Seq.map (Seq.map getOuterHtml)