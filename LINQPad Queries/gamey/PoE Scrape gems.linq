<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json</Namespace>
  <Namespace>System.Net</Namespace>
  <Namespace>System.Net.Http</Namespace>
</Query>

let delimit (d:string) (s:string seq) = String.Join(d,values=s)
let replace (s:string) (r:string) (t:string) = t.Replace(s,r)

let (|ParseInt|_|) (x:string) =
    match Int32.TryParse x with
    | true,i -> Some i
    | _ -> None
let (|ValueString|NonValueString|) (x:string) = 
    if String.IsNullOrWhiteSpace(x) then
        NonValueString
    else ValueString (x.Trim())

let (|RMatch|_|) p x =
    let m = Regex.Match(x,p)
    if m.Success then
        Some m
    else None
    
module SuperSerial =
    let serialize : obj -> string = JsonConvert.SerializeObject
    let deserialize: string -> 't = JsonConvert.DeserializeObject
let text = 
    let f () =
        use hc = new HttpClient()
        hc.GetStringAsync("https://pathofexile.gamepedia.com/Skill_gem").Result
    Util.Cache(f, "html")
let parsed = 
    let doc = new HtmlAgilityPack.HtmlDocument()
    doc.LoadHtml text
    doc
//let leagues =
//    let span = parsed.GetElementbyId "Past_leagues"
//    span.ParentNode.NextSibling.NextSibling.ChildNodes
//    |> Seq.map(fun cn -> cn.InnerText)
//    |> Seq.filter(String.IsNullOrWhiteSpace >> not)
//    |> Seq.choose ((|RMatch|_|) (sprintf "The (\w+) league .* %s to %s" mdyPattern mdyPattern))
//    |> Seq.map(fun m -> m.Groups.[1].Value, DateTime.Parse m.Groups.[2].Value, DateTime.Parse m.Groups.[3].Value)
//    |> Seq.map(fun (name,start,end_) -> name, end_ - start,start,end_)

//let getInnerHtml (x:HtmlNode) = x.InnerHtml
//let hoverableSpan = "//span[@class='c-item-hoverbox' and .//span[-gem]]"
//let hoverableSpan = "//span[@class='c-item-hoverbox' and .//span[starts-with(@class,'active')]]"
//let hoverableSpan = "//span[@class='c-item-hoverbox' and .//span[@class='item-box -gem']]//span[@class=header -single]"
let hoverableSpan = "//span[@class='c-item-hoverbox' and .//span[@class='item-box -gem']]"
type Gem = {Name:string; Level:int; Description:string list}
let (|GemNode|_|) (x:HtmlAgilityPack.HtmlNode) =
//    x.NodeType = HtmlAgilityPack.HtmlNodeType.Element
    if x.Name="span" && x.ParentNode.ParentNode.Name = "tr"
        && x.GetClasses() |> Seq.contains("c-item-hoverbox")
        then
        //&& x.SelectNodes("//span[@class='header -single']")
        let name= x.SelectNodes(".//span[@class='header -single']") |> Seq.tryHead |> Option.map(fun x -> x.InnerText)
        let lvl = 
            match x.ParentNode.ParentNode.SelectSingleNode("./td[@class='tc -value']") with
            | null -> -1
            | lvlNode ->
                match lvlNode.InnerText with
                | ParseInt i -> i
                | x -> 
                    eprintfn "%s-%s" lvlNode.Name lvlNode.InnerText
                    -2
            
        let y = ()
        match name with
        | Some name ->
            Some {Name=name;Level=lvl; Description=[]}
        | None -> None
    else None
    
type OuputType =
    | Names
    | Full
let getActiveSkills ot =
    if isNull parsed then failwithf "bad parse?"
    if isNull parsed.DocumentNode then failwithf "bad doc?"
    let span =
        match parsed.DocumentNode.SelectNodes(hoverableSpan) with
        | null ->
            text.Dump("text")
            failwithf "nothing found"
        | x ->
            x
            |> Seq.choose (|GemNode|_|)
            |> Seq.distinct
            |> fun x ->
                match ot with
                | Names ->
                    x
                    |> Seq.sort
                    |> Seq.map (fun g -> g.Name |> replace "'" "\\'")
                    |> Seq.map(sprintf "'%s',")
                    |> delimit Environment.NewLine
                | Full ->
                    x 
                    //                     |> Seq.map (fun (x,y) ->  x |> replace "'" "\\'", y)

                    |> Seq.map (fun g ->  g.Name, g)
                    
//                    |> Seq.map(fun (x,y) -> sprintf "'%s',%i" x y)
                    |> Map.ofSeq
                    |> SuperSerial.serialize
    //        |> Seq.map(fun x -> x.InnerHtml)
    span
getActiveSkills Full
|> Dump
|> ignore
//text
//|> Dump
//|> ignore