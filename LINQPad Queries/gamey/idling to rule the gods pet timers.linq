<Query Kind="FSharpProgram" />

// pet cutoffs, how long until next cutoff?
[<Measure>] type Hour
[<Measure>] type Hunger
//let hungerPerPercent = FullBar / 100.<Hunger>
let hoursPerHungerPercent =  12.<Hour> / 100.<Hunger>
let hungerPerHour = 100.<Hunger> / 12.<Hour>

let hunger = 86.07<Hunger>
let displayHours (h:float<Hour>) = 
    
    let ts = 
        let hoursCalc = int h
        let minCalc = (float h - (h |> int |> float)) * 60. |> int
        let result = TimeSpan( hoursCalc, minCalc, 0)
        printfn "Hours:%f; CalcHours:%i; CalcMins:%i; Timespan:%A" h hoursCalc minCalc result
        (h,hoursCalc, minCalc, result).Dump()
        result
    
    sprintf "%i Hour(s) %i minutes" ts.Hours ts.Minutes
hoursPerHungerPercent * 1.<Hunger>
|> displayHours 
|> fun x -> x.Dump("Hours per hunger %")

(
//let starvationIn =
    hoursPerHungerPercent * hunger
    //|> Dump
    |> displayHours
    |> fun x -> x.Dump("Starvation timer")
    |> ignore
)

//let displayHours = 
type FeedingBonus = {HoursFromFull:float<Hour>; HungerRequired: float<Hunger>; BonusSize: float;}
let cutoffs = 
    [ 
        3.<Hour> , 75.<Hunger> , 0.25
        6.<Hour> , 50.<Hunger> , 0.5
        10.8<Hour>, 10.<Hunger> , 1. ]
    |> List.map (fun (f,h,b) -> {HoursFromFull = f; HungerRequired = h; BonusSize = b})
    
cutoffs
// skip cutoffs already passed
|> Seq.filter(fun fb -> 
    // now, other intervals
    fb.HungerRequired < hunger
)
|> Seq.map (fun fb ->
    hoursPerHungerPercent * (hunger - fb.HungerRequired) |> displayHours, fb
)
//|> Seq.map 

|> Dump