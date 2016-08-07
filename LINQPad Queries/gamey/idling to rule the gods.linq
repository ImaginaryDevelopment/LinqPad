<Query Kind="FSharpProgram" />

// math from http://www.kongregate.com/forums/2874-idling-to-rule-the-gods/topics/454405-god-power-currency-guide
[<Measure>] type Hours

//[<Measure>] type TotalBuildingSpeedMultiPercentage // 415% for instance
let actualBuildTime clones totalBuildingSpeedMultiplier (baseBuildTime:float<Hours>)=
    let b,c,m = baseBuildTime , clones, totalBuildingSpeedMultiplier
    let rawResult = (b/(m/100.))/(c/1000.)
    // TimeSpan(Math.Round rawResult |> int)
    rawResult

let getTotalHours days (hours:int<Hours>) minutes seconds :float<Hours>= 
    TimeSpan(days,int hours,minutes,seconds).TotalHours * 1.<Hours>
    
let formatTS (ts:TimeSpan) = 
    String.Format("{0} hour{1} {2} minute{3}", 
              ts.Hours, 
              (if ts.Hours = 1 then "" else "s"),
              ts.Minutes, 
              (if ts.Minutes = 1 then "" else "s"))    
let getFormattedBuildTime clones b baseBuildTime =
    actualBuildTime clones b baseBuildTime
    |> float 
    |> TimeSpan.FromHours
    |> formatTS


//sample from game Temple of god level 6

getFormattedBuildTime 90000. 415. 2000.<Hours>
|> Dump
//sample from game Temple of god level 6

getTotalHours 1 8<Hours> 40 47
|> getFormattedBuildTime 1000. 665.
|> Dump

getTotalHours 1 8<Hours> 40 47
|> getFormattedBuildTime 1000. (665. + 87.5)
|> Dump