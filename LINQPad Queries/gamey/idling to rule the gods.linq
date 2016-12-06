<Query Kind="FSharpProgram" />

// math from http://www.kongregate.com/forums/2874-idling-to-rule-the-gods/topics/454405-god-power-currency-guide
[<Measure>] type Hour

//[<Measure>] type TotalBuildingSpeedMultiPercentage // 415% for instance
let actualBuildTime (clones:int) (totalBuildingSpeedMultiplier:float) (baseBuildTime:float<Hour>)=
    let b,m = baseBuildTime , totalBuildingSpeedMultiplier
    let c = float clones
    let rawResult = (b/(m/100.))/(c/1000.)
    // TimeSpan(Math.Round rawResult |> int)
    rawResult
let getBaseBuildTime totalBuildingSpeedMultiplier (actualBuildTimeTooltip:float<Hour>) = 
    // actual = (b/(m/100.))/(c/1000.)
    // c is 1000 so that term drops
    // actual = b / (m/100)
    // b = actual * (m/100)
    
    let rawResult = actualBuildTimeTooltip * (totalBuildingSpeedMultiplier/ 100.)
    rawResult
let getTotalHours days (hours:int<Hour>) minutes seconds :float<Hour>= 
    TimeSpan(days,int hours,minutes,seconds).TotalHours * 1.<Hour>
    
let formatTS (ts:TimeSpan) =
    let beforeHours = 
        String.Format("{0} hour{1} {2} minute{3}", 
              ts.Hours, 
              (if ts.Hours = 1 then "" else "s"),
              ts.Minutes, 
              (if ts.Minutes = 1 then "" else "s"))    
    if ts.Days < 1 then
        beforeHours 
    else sprintf "%i%s, %s" ts.Days (if ts.Days > 1 then "days" else "day") beforeHours
let formatHours (h:float<Hour>) : string =
    float h
    |> TimeSpan.FromHours
    |> formatTS
    
let getFormattedBuildTime clones b baseBuildTime =
    actualBuildTime clones b baseBuildTime
    |> float 
    |> TimeSpan.FromHours
    |> formatTS


//sample from game Temple of god level 6

getFormattedBuildTime 90000 415. 2000.<Hour>
|> Dump
//sample from game Temple of god level 6

getTotalHours 1 8<Hour> 40 47
|> getFormattedBuildTime 1000 665.
|> Dump

getTotalHours 1 8<Hour> 40 47
|> getFormattedBuildTime 1000 (665. + 87.5)
|> Dump
let readValues () = 
    let input = Util.ReadLine("Please input the tooltip build time (days, hours, minutes, seconds)")
    if String.IsNullOrWhiteSpace input then
        None
    else
        match input.Split([| " " |], StringSplitOptions.RemoveEmptyEntries) |> Seq.map int |> List.ofSeq  with
        | [m;s] -> Some (input, 0, 0<Hour>, m, s)
        | [h;m;s] -> Some (input, 0,h * 1<Hour>,m,s)
        | [d;h;m;s] -> Some (input, d,h * 1<Hour>,m,s)
type Foo = { InputHours:string; Clones:int;RawInput:string; BaseBuildTime: string; ClonesAppliedBuildTime: string}    
let dumper = 
    let dc = DumpContainer()
    dc.Dump()
    dc.Content <- []
    (fun item -> dc.Content <- box item::(dc.Content :?> _ list))
    
// clones dedicated to building not all clones0
let clones = 10000
//totalBuildTimeMultiplier 
let tbt = 805.

()
|> Seq.unfold(fun () ->
    match readValues() with
    | Some (input, d,h,m,s) -> 
        let result = 
            let th = getTotalHours d h m s 
            getBaseBuildTime tbt th
            |> fun x-> {BaseBuildTime=formatHours x; RawInput=input; InputHours= formatHours th; Clones=clones; ClonesAppliedBuildTime=getFormattedBuildTime clones tbt x}
        dumper result
        Some(result,())
    | None -> None
)
|> Dump
|> ignore

// appears to work!