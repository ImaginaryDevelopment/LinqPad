<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Core</NuGetReference>
</Query>

let replace (d) (r) = function | null|"" as x -> x | x -> x.Replace(oldValue=d,newValue=r)
module TimeSpan =
    let getTotalMinutes (ts:TimeSpan) = ts.TotalMinutes
    let toNearestMinutes minutes (ts:TimeSpan) =
        let totalMinutes = getTotalMinutes (ts + TimeSpan(0,minutes / 2, 0)) |> Convert.ToInt32
        TimeSpan(0,totalMinutes - totalMinutes % minutes, 0)
module Tuple2 =
    let map f (a,b)= f a, f b
    let rotate (a,b) = b,a
    let combine f (a,b) = f a b
[
    "Nov 29, 2018 at 5:03 PM", "Nov 29, 2018 at 5:48 PM"
    "Nov 29, 2018 at 6:41 PM", "Nov 29, 2018 at 8:26 PM"
]
|> Seq.map (Tuple2.map (replace "at " ""))
|> Seq.append (
    [
        "Jan 22 at 4:33 PM", "Jan 22 at 6:36 PM"
        "Jan 24 at 4:32 PM", "Jan 24 at 7:31 PM"
        "Jan 24 at 8:38 PM", "Jan 24 at 9:15 PM" // the corrected value from not moving it back until the next day
        "Jan 28 at 4:02 PM", "Jan 28 at 7:39 PM"
        "Jan 29 at 4:01 PM", "Jan 29 at 8:45 PM"
        "Feb 1 at 8:54 AM", "Feb 1 at 10:37 AM"
        "Feb 4 at 8:52 AM", "Feb 4 at 1:43 PM"
        "Feb 4 at 2:25 PM", "Feb 4 at 2:58 PM"
        "Feb 4 at 5:02 PM", "Feb 4 at 7:15 PM"
        "Feb 6 at 11:40 AM", "Feb 6 at 2:20 PM"
        "Feb 7 at 8:53 AM", "Feb 7 at 11:33 AM"
        "Feb 11 at 12:58 PM", "Feb 11 at 6:45 PM"
        "Feb 12 at 8:57 AM", "Feb 12 at 11:14 AM"
        "Feb 12 at 1:25 PM", "Feb 12 at 2:19 PM"
        "Feb 12 at 2:45 PM", "Feb 12 at 7:15 PM"
        "Feb 13 at 8:35 AM", "Feb 13 at 5:35 PM"
        "Feb 14 at 9:00 AM", "Feb 14 at 1:15 PM"
        "Feb 15 at 1:18 PM", "Feb 15 at 2:50 PM"
        "Feb 15 at 3:24 PM", "Feb 15 at 6:00 PM"
        "Feb 18 at 8:21 AM", "Feb 18 at 9:15 AM"
        "Feb 18 at 12:55 PM", "Feb 18 at 2:00 PM" // offline stuff
        // meeting snipping 15 from next time
        "Feb 18 at 2:15 PM", "Feb 18 at 3:00 PM" // snipped out
    ]
    |> Seq.map(Tuple2.map(replace "at " ", 2019 "))
)
|> Dump
|> Seq.map(Tuple2.map DateTime.Parse)
|> Seq.map(fun (st,en) -> st,en,en - st)
|> Seq.fold(fun (items,(rawTotal,roundedTotal)) (st,en,ts) ->
    let ts' = TimeSpan.toNearestMinutes 15 ts
    (st,en,ts',ts)::items, (rawTotal + ts,roundedTotal + ts')
    
) (List.empty,(TimeSpan 0L, TimeSpan 0L))
|> fun (x,(t,rndt)) ->
    x,(sprintf "%i:%02i" (t.Days * 24 + t.Hours) t.Minutes, sprintf "%i:%02i" (rndt.Days * 24 + rndt.Hours) rndt.Minutes)
|> Dump
|> ignore