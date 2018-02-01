<Query Kind="FSharpProgram">
  <Connection>
    <ID>01133a54-bf1f-423a-b144-12acb2f6c661</ID>
    <Persist>true</Persist>
    <Server>prog7-pc</Server>
    <SqlSecurity>true</SqlSecurity>
    <Database>hd</Database>
    <UserName>sa</UserName>
    <Password>AQAAANCMnd8BFdERjHoAwE/Cl+sBAAAAry/3yE9NCE6DnvfcIFtbCgAAAAACAAAAAAAQZgAAAAEAACAAAAAzy5gld67tCJJ0CzChoWaRaEonLQEL3WErGtHeNySfwwAAAAAOgAAAAAIAACAAAAC9pt88JJEbzEjGDn6avASLQDKHkH/DveHuj8xkoqzkWBAAAACYic7/FZCjyP1xQd6Og7HzQAAAALpwyi8RHLgOTRA+RpUCxw5uvupEffoF2gS5rVFbIY5AWvTq2iAcmNgU69C3fHSUC3Anh6wKyNK8QOTsxR/UHE4=</Password>
  </Connection>
  <NuGetReference>FSharp.Core</NuGetReference>
</Query>

let dc = new TypedDataContext()

// calculate business days between x and y

let start = DateTime(2018,1,9)

let stop = DateTime(2018,2,6)


let rawDayCount = stop - start
let days = 
    start
    |> Seq.unfold(fun day  ->
        if day < stop && day.DayOfWeek <> DayOfWeek.Sunday && day.DayOfWeek <> DayOfWeek.Saturday then
            Some(Some day, day.AddDays 1.)
        elif day >= stop then
            None
        else Some(None, day.AddDays 1.)
    )
    |> Seq.choose id
    // days it was not in doing
    |> Seq.filter(fun dt -> dt.Day <> 22)
    |> List.ofSeq
let businessDayCount = days |> List.length

days
|> Seq.map(fun dt -> dt, dt.DayOfWeek.ToString())
|> Dump
|> ignore


(businessDayCount, businessDayCount * 8, businessDayCount * 8 * 80, 130 * 80).Dump("days,totalHours,total$")