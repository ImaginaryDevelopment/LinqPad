<Query Kind="FSharpProgram">
  <Reference>D:\projects\ToMEHelper\src\ToMEHelper\bin\Debug\net5.0\ToMEHelper.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
  <Namespace>System.Text.Json</Namespace>
  <Namespace>ToMEHelper.Scraping.ApiSamples.AnalysisSchema</Namespace>
</Query>

let deserialize<'t>(x:string) =
    Newtonsoft.Json.JsonConvert.DeserializeObject<'t>(x)
// are these always redundant or just for specific classes?
let redundantTalents = [
    "Chant of Fortitude" // in talent Chant Acolyte
    "Chant of Fortress"
    ]
let data = 
    let sourceQueryPath = "ToMEHelper data download"
    let tomeLPath =
        let cqp = Util.CurrentQueryPath
        Path.GetFullPath cqp |> Path.GetDirectoryName
    let gameyPath =  tomeLPath |> Path.GetDirectoryName
    let tomeDataPath = Path.Combine(gameyPath, "Data", tomeLPath |> Path.GetFileName)
    let p = Path.Combine(tomeDataPath, sourceQueryPath |> sprintf "%s.json")
    if File.Exists p then
        File.ReadAllText p
        |> deserialize<CharAnalysisData>
    else
        failwithf "No file found at '%s'" p
        
let chars = data.Chars |> Array.map(fun (c,cas) -> c, cas |> Array.map(fun cas -> {cas with Talents = cas.Talents |> List.filter(fun (n,_) -> redundantTalents |> List.contains n |> not)} ) )
let catMap =
    (Map.empty, chars)
    ||> Seq.fold(fun m (a,ctc) ->
        (m, ctc)
        ||> Seq.fold(fun m ct ->
            let next =a,ct.Mastery,ct.Talents
            match m |> Map.tryFind ct.Category with
            | None -> m |> Map.add ct.Category [next]
            | Some items -> m |> Map.add ct.Category (next::items)
        )
    )

type CategoryInfo = 
    {
        Count:int
        Pct: float
        // how many talents purchased total
        TalentCount: int
        // how many talents in category purchased
        DepthAvg: float
        // of talents purchased, average points invested
        MasteryMin: float
        MasteryMax: float
        // how many are above min
        MasteryCount: int
    }
let investment =
    catMap
    |> Map.toSeq
    |> Seq.map(fun (k, items) ->
        let mas = items |> List.map(fun (_,m,_) -> m)
        let mm = mas |> List.min
        k, {
            Count= items.Length
            Pct = float items.Length / float chars.Length
            TalentCount = items |> List.map(fun (_,_,ts) -> ts.Length) |> Seq.sum
            DepthAvg = items |> Seq.map(fun (_,_,ts) -> float ts.Length) |> Seq.average
            MasteryMin = mm
            MasteryMax = mas |> List.max
            MasteryCount = mas |> List.filter(fun v -> v > mm) |> List.length
        }
    )
(investment |> Seq.sortByDescending(fun (k,v) -> v.Pct, v.DepthAvg, k) |> Seq.map(fun (k,v) -> k, sprintf "%A" v)).Dump()
type TalentSum = {
    Ch: CharSummary
    Mastery: float
    Value: float
}
let tMap =
    chars
    |> Seq.collect(fun (ch,ts) ->
        ts
        |> Seq.collect(fun x ->
            x.Talents
            |> Seq.map(fun (n,v) -> x.Category,n, {Ch=ch;Value= v;Mastery= x.Mastery})
        )
    )
    |> Seq.groupBy(fun (c,_,_) ->
        c
    )
    |> fun x -> x
    |> Seq.map(fun (k, x) ->
        k,
            x
            |> Seq.map (fun (_,n,v) -> n,v)
            |> Seq.groupBy fst
            |> Seq.map(fun (k,v) ->
                k,
                    v |> Seq.map snd |> Seq.sortDescending
            )
            |> Seq.sortByDescending(fun (k,v) -> v |> Seq.map(fun x -> x.Value) |> Seq.sum, k)
            |> List.ofSeq
    )
    |> Seq.map(fun (k,v) ->
        let catAvg = v |> Seq.collect snd |> Seq.map(fun item -> item.Value) |> Seq.average
        k,(catAvg,v |> Seq.map(fun (tName,items) -> tName, items |> Seq.averageBy(fun item -> item.Value), items))
    )
    |> Map.ofSeq
tMap.Dump("talents")
// category -> characters with category count
let didinvest =
    (Map.empty, chars)
    ||> Seq.fold(fun m (_,b) ->
        (m,b)
        ||> Seq.fold(fun m ca ->
            match m |> Map.tryFind ca.Category with
            | Some count -> m |> Map.add ca.Category (count + 1)
            | None -> m |> Map.add ca.Category 1
        )
    )
    |> Map.toSeq
    
    
let getAvgChars (count:int) = float count / float chars.Length * 100. |> sprintf "%.0f"
didinvest
|> Seq.sortBy (fun (k,v) -> -1 * v, k)
|> Seq.map(fun (t,ct) ->
    sprintf "%-33s%3i %5s%%" t ct (getAvgChars ct)
)
|> String.concat "\r\n"
|> sprintf "<pre>%s</pre>"
|> Util.RawHtml
|> fun x -> x.Dump("talents")
|> ignore


//|> Seq.map(fun (char,cs) ->
//)

data.Filter.Dump("filter")