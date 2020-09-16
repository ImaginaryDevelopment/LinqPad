<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json</Namespace>
  <Namespace>System.Net</Namespace>
  <Namespace>System.Net.Http</Namespace>
</Query>

// parse poe gamepedia for the active skill gems, with required level
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
    let inline deserialize (x:string) = JsonConvert.DeserializeObject x
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
        let almostThere =     
                x.ParentNode
                    .SelectSingleNode(".//span[@class='item-box -gem']")
        let description = 
            try
                        almostThere.SelectNodes(".//span[contains(@class,'group') and contains(@class, 'tc')]")
                        |> Seq.map(fun x -> x.InnerHtml)
                        |> List.ofSeq
            with _ ->
                printfn "Failed on %A" name
                almostThere.InnerHtml.Dump()
                reraise()
        match name with
        | Some name ->
            Some {  Name=name;Level=lvl
                    Description= description}
        | None -> None
    else None
    
type OuputType =
    | Fiddle
    | Full
type Skill = {Name:string; ReqLvl:int Nullable}
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
                | Fiddle -> // https://jsfiddle.net/Maslow/e02ts1jy/show
                    x
                    |> Seq.map(fun x -> {Name=x.Name; ReqLvl=Nullable x.Level})
                    |> Seq.sortBy(fun x -> x.Name)
                    |> SuperSerial.serialize
                | Full ->
                    let d =
                        x 
//                        |> Seq.map (fun g ->  g.Name, g)
//                        |> Map.ofSeq
                    d.Dump()
                    d
                    |> SuperSerial.serialize
    //        |> Seq.map(fun x -> x.InnerHtml)
    span
getActiveSkills Full
|> fun x -> System.Windows.Forms.Clipboard.SetText x; x
|> Dump
|> ignore
//text
//|> Dump
//|> ignore