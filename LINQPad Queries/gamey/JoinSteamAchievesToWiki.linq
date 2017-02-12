<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>FSharp.Data</NuGetReference>
  <Namespace>FSharp.Data</Namespace>
  <Namespace>Microsoft.FSharp.Core</Namespace>
</Query>

let statsUrl = "http://steamcommunity.com/id/maslow/stats/appid/250900/achievements"
let achievesUrl = "http://bindingofisaacrebirth.gamepedia.com/Achievements"
let dumpt t x = x.Dump(description=t); x
let useCache = true
let cache title f = 
    if useCache then
        Util.Cache(Func<_>(f), title)
    else f()
let load title (uri:string) = 
    cache title (fun () -> FSharp.Data.HtmlDocument.Load uri)
let statsHtml = 
    load "statsHtml" statsUrl
let achievesHtml = 
    load "achievesHtml" achievesUrl
type StatRow = { Img:obj; Title:string; Subtitle:string; UnlockTime:string; ImgUrl:string} with 
    member x.WithImg (f:unit -> obj) :StatRow = {x with Img= f() }
//type AchRow = {
type HtmlNode with
    static member innerText (h:HtmlNode) = h.InnerText
[<AutoOpen>]
module Domain = 
    let select selector (h:HtmlNode) = h.CssSelect selector
    let getInnerText (h:HtmlNode) = h.InnerText()
    let getAttrib s (h:HtmlNode) = h.AttributeValue s
    let dump x = x.Dump(); x
    let Dump x = x.Dump()
    let equalsI (s:string) (s2:string) = String.Equals(s,s2, StringComparison.InvariantCultureIgnoreCase)
    let trim (s:string) = s.Trim()
module Seq = 
    let takeUpTo count (items:'T seq) : 'T seq = items |> Seq.mapi (fun i x -> i,x) |> Seq.takeWhile (fst >> flip (<) (count)) |> Seq.map snd
    
//module Seq =
//    let tryHead items = items |> Seq.tryFind(fun _ -> true)
let getIntermediaryImages = false
let statsRowsRaw = statsHtml.CssSelect ".achieveRow" |> List.ofSeq
let statsRows =
    statsRowsRaw
    |> List.map (fun h -> 
        let imgUrl = h.CssSelect("img[src]") |> Seq.tryHead |> Option.map (getAttrib "src")
        {
            ImgUrl=imgUrl |> Option.toNull
            Title=h.CssSelect "h3" |> Seq.tryHead |> Option.map (getInnerText>>trim) |> Option.toNull
            Subtitle= h.CssSelect "h5" |> Seq.tryHead |> Option.map getInnerText |> Option.toNull
            UnlockTime = h.CssSelect ".achieveUnlockTime" |> Seq.tryHead |> Option.map getInnerText |> Option.toNull
            Img = if getIntermediaryImages then imgUrl |> Option.map Util.Image |> Option.toNull |> box else null
        }
    )

type AchieveRow = {Img:obj; Title:string; Subtitle:string; Req:string; Category:string; ImgUrl:string;} with
    member x.WithImg (f:unit -> obj) :AchieveRow = {x with Img = f()}
let achieveRowsRaw =
    achievesHtml.CssSelect "table[data-description]" 
    |> Seq.map (fun table -> table.CssSelect "tr" |> Seq.skip 1 |> List.ofSeq |> List.map(fun h -> table |> getAttrib "data-description", h))
    |> List.ofSeq
// grouped by Category
let achieveRows =
    let mapRow category h =
        let imgUrl = h |> select "img[src]" |> Seq.tryHead |> Option.map (getAttrib "src") 
        {
            ImgUrl=imgUrl |> Option.toNull
            Title=h |> select "td" |> Seq.tryHead |> Option.map (getInnerText>>trim) |> Option.toNull
            Subtitle= h |> select "td" |> Seq.skip 2 |> Seq.tryHead |> Option.map getInnerText |> Option.toNull
            Req = h |> select "td" |> Seq.skip 3 |> Seq.tryHead |> Option.map getInnerText |> Option.toNull
            Img = if getIntermediaryImages then imgUrl |> Option.map Util.Image |> Option.toNull |> box else null
            Category = category            
        }
        
    achieveRowsRaw
    |> Seq.map (Seq.map (fun (category,h) ->
        try
            mapRow category h
        with _ -> 
            h |> string |> Dump |> ignore
            reraise()
    ))
    |> Seq.collect id
    |> List.ofSeq
    
sprintf "Achievements: %i, StatsRows: %i" (achieveRows.Count()) (statsRows.Count()) |> Dump |> ignore
let achieveDisplay () =
    achieveRows 
    |> Seq.groupBy (fun x -> x.Category)
    |> Seq.map (fun (category,rows) -> category, (rows |> Seq.take 5 |> List.ofSeq))
    |> List.ofSeq
    
let inline fillImg (x:^T) : ^T = 
    let imgUrl = (^T: (member ImgUrl: string) x) |> Util.Image |> box
    let updated = (^T: (member WithImg: (unit -> obj) -> ^T ) x, (fun () -> imgUrl)) 
    updated

//let joined = 
let areMates (ar:AchieveRow) (sr:StatRow) = 
    equalsI ar.Title sr.Title
    || [ 
        "Book of Revelations", "The Book of Revelations"
        "Everything is Terrible!!!", "Everything is Terrible!!!"
        "Cube of Meat", "A Cube of Meat"
    ] |> Seq.exists(fun (arTitle,srTitle) -> equalsI ar.Title arTitle && equalsI srTitle sr.Title)
achieveRows
|> Seq.map(fun ar -> ar, statsRows |> Seq.tryFind(areMates ar))
|> Seq.filter(snd >> function | None -> true | Some sr -> String.IsNullOrEmpty sr.UnlockTime)
|> Seq.map fst
|> Seq.takeUpTo 50
//|> dump
//|> Seq.map (fun x -> {x with Img = x.ImgUrl |> Option.ofNull |> Option.map Util.Image |> Option.toNull})
|> Seq.map fillImg
|> dumpt "unachieved"

type UnlockStatus = 
    | Unlocked of DateTime option
    | Locked
    | Unchecked
    
let tryMate ar = 
    statsRows 
    |> Seq.tryFind (areMates ar) 
    |> function 
        | Some sr-> if String.IsNullOrEmpty sr.UnlockTime then 
                        Locked 
                    else 
                        sr.UnlockTime 
                        |> DateTime.TryParse 
                        |> function 
                            | true, dt -> Some dt 
                            | false, _ -> None
                            |>  Unlocked
                    |> Some
        | None -> None

Util.ReadLine("Search achievements for?",null, achieveRows |> Seq.map(fun ar -> ar.Title))
|> fun searchTerm -> 
    achieveRows |> Seq.tryFind(fun ar -> ar.Title = searchTerm) 
    |> function 
        |Some achievement -> (fillImg achievement, tryMate achievement) |> Dump |> ignore
        |None -> 
            // fuzzier searches
            let fuzzier = 
            
                achieveRows 
                |> Seq.filter(fun ar -> ar.Title |> containsI searchTerm || ar.Req |> containsI searchTerm)
                |> Seq.map (fun ar -> ar |> fillImg, tryMate ar |> sprintf "%A")
                |> List.ofSeq
            match fuzzier with
            | [] -> statsRows |> Seq.tryFind(fun sr -> sr.Title = searchTerm) |> Dump |> ignore
            | x -> x |> Dump |> ignore

Util.OnDemand("Raw sample stats rows from steam", 
    (fun () -> 
        statsRowsRaw
        |> Seq.takeUpTo 5
        |> Seq.map string))
|> Dump

Util.OnDemand("Raw sample achievement rows from wiki", 
    (fun () -> 
        achieveRowsRaw
        |> Seq.takeUpTo 5
        |> Seq.map string))
|> Dump