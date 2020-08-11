<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll</Reference>
  <NuGetReference>FSharp.Data</NuGetReference>
  <Namespace>FSharp.Data</Namespace>
</Query>

[<Literal>]
let path = @"D:\Users\DBee\Downloads\stmt.csv"
[<Literal>]
let sample ="Date,Description,Amount,RunningBal\r\n\"01/01/2017\",\"hello world\",\"23.50\",\"$2343.28\""
type StProv = CsvProvider<sample,IgnoreErrors=true, Schema="Date:DateTime, Description:string, Amount decimal, RunningBal: decimal"> 

let flip f x y = f y x
let st = StProv.Parse(File.ReadAllLines path |> Seq.skip 5 |> delimit "\r\n")
let gamePurchases = 
    st.Rows
    |> Seq.filter(fun r -> 
        r.``Amount decimal`` < 0m 
        && not <| containsI "bank" r.``Description:string``
        && not <| containsI "childs place" r.``Description:string``
        && not <| containsI "navient" r.``Description:string``
        && not <| containsI "capital one" r.``Description:string``
        && not <| containsI "sav 3168" r.``Description:string``
    )
    |> Seq.sortBy(fun r -> r.``Date:DateTime``)
    |> List.ofSeq
let min,max = 
    gamePurchases |> Seq.map (fun r -> r.``Date:DateTime``)
    |> fun r -> r |> Seq.min, r |> Seq.max
let days = (max - min).TotalDays
type Stats = {CountPerDay: decimal; CountPerWeek:decimal; CountPerMonth:decimal; AmountPerDay:decimal; AmountPerWeek:decimal; AmountPerMonth:decimal; MinDay:DateTime;MaxDay:DateTime}
let avgPerDay = gamePurchases |> Seq.length |> decimal |> flip ( / ) (decimal days)
let amtPerDay = gamePurchases |> Seq.map (fun r -> r.``Amount decimal``) |> Seq.sum |> flip (/) (decimal days)
{CountPerDay= avgPerDay; CountPerWeek= avgPerDay * 7m; CountPerMonth= avgPerDay * 28.5m; AmountPerDay = amtPerDay; AmountPerWeek= amtPerDay * 7m; AmountPerMonth= amtPerDay * 28.5m; MinDay = min; MaxDay=max}
|> Dump
|> ignore
//(avgPerDay, avgPerDay * 7m, avgPerDay * 28.5m).Dump()
gamePurchases
|> Dump
|> ignore