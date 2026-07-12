<Query Kind="FSharpProgram">
  <NuGetReference>Humanizer.Core</NuGetReference>
  <Namespace>Humanizer</Namespace>
</Query>

// CIFI time to next loop

// how many ticks on the reset page do you have, how many needed to finish next loop, how fast is a tick?
let ticks, ticksReq, tickRate = 17, 634, 3.9
// how many cells on the reset page do you have, how many for next loop, how fast are they coming in?
let cells, cellsReq, cellRate = 2.33e163, 3.85e164, 4.37e161

let million = 1_000_000.0
let billion = million * 1_000.0
let trillion = billion * 1_000.0
let qa = trillion * 1_000.0
let qu = qa * 1_000.0
// player menu
let ticksThisLoop = 2316

// reset menu
let resetModPoints = 2.17 * float qa
let nextLoopModPoints = 164.06 * float trillion
type Resource = {
    Rate: float
    Inventory: float } // consider uint64?
let dumpAction =
    let dc = DumpContainer()
    dc.Dump("suggestion")
    fun (value:obj) ->
        dc.Content <- value
let cellsR = {
    Rate= cellRate
    Inventory= cells
}


//let ticksNeeded = ticksReq - ticks
//let timeToLoop = float ticksNeeded * secondsPerTick / 60.0

let createSummary title (resource:Resource) ticksReq =
    let seconds = ticksReq * tickRate
    {| Title= title; TicksNeeded= ticksReq; Seconds= seconds; Minutes = seconds / 60.0; Resource= resource |}
    
let tickLoop =
    //createSummary "Time" ticks ticksReq
    createSummary "Time" cellsR ticksReq
    
let cellLoop =
    let cellTicksNeeded = (cellsReq - cellsR.Inventory) / cellsR.Rate
    //let timeToCellLoop =
    //    cellTicksNeeded * secondsPerTick / 60.0
    createSummary "Cells" cellsR cellTicksNeeded
[ tickLoop;cellLoop] |> ignore
    
let indexOf (delimiter:string) (value:string) =
    let i = value.IndexOf delimiter
    if i>= 0 then Some i
    else None

let (|Before|_|) (delimiter:string) value =
    match value |> indexOf delimiter with
    | Some i -> Some value[0..i - 1]
    | _ -> None
    
let (|After|_|) (delimiter:string) (value:string) =
    match value |> indexOf delimiter with
    | Some i -> Some value[i + delimiter.Length ..]
    | None -> None
    
// not doing multiples
let remove (delimiter:string) =
    function 
    | Before delimiter b & After delimiter a -> b + a
    | x -> x
let prettifySeconds seconds =
    [
        if seconds <= 120. then
            yield $"%.1f{seconds} seconds"
        else
            let minutes = seconds / 60.
            if minutes <=60. then
                let minutes,seconds = Math.Truncate(minutes), seconds % 60.
                yield $"%.0f{seconds}"
                yield $"%.0f{minutes} minutes"
            else
                let hours, minutes = Math.Truncate(minutes / 60.),minutes % 60.
                yield $"%.1f{minutes % 60.} minutes"
                if hours <= 24. then
                    yield $"%.0f{hours} hours"
                
                if hours > 48. then
                    let days = hours / 24.
                    yield $"%.1f{days} days"
    ]
    |> List.rev
    |> List.truncate 2
    |> String.concat " - "
    
let summarizeWait (title: string) goal resource (asof: DateTime) =
    if resource.Inventory > goal then
        title, asof, None, "Ready"
    else
    let seconds = float (goal - resource.Inventory) / float resource.Rate
    let eta = asof.AddSeconds(seconds)
    title, asof, Some eta, prettifySeconds seconds
        

// start efficiency calc

let inline prettifyE (c:float) = c.ToString("e2") |> remove "+0" |> remove "+" 

let secondsThisLoop = float ticksThisLoop * tickRate

let nextLoopCause =
    [tickLoop;cellLoop]
    |> Seq.minBy(fun v -> v.Minutes) 

let value = {| TimeNeededMinutes = nextLoopCause.Minutes; Tick= tickLoop; Cell= cellLoop; ModPoints= prettifyE resetModPoints |}

value.Dump("Estimate till next")
let getRate amount time =
    amount / time

// how fast on average have you been earning mp?
let runEarnRate = getRate resetModPoints secondsThisLoop
printfn "SecondsThisLoop: %A" secondsThisLoop
printfn "%s earned in %s" (prettifyE resetModPoints) (prettifySeconds secondsThisLoop)

let nextLoopValueEstimate = nextLoopModPoints / value.TimeNeededMinutes

//summarizeWait "loop" runEarnRate nextLoopCause.Resource DateTime.Now
//|> Dump
//|> ignore
resetModPoints.ToMetric().Dump("reset")
let dumpRateSummary title (mp:float) (minutes:float) =
    let rate = mp / minutes
    sprintf "%s: %s at %s per minute(%s)" title (prettifyE mp) (prettifyE rate) (rate.ToMetric(MetricNumeralFormats.UseShortScaleWord, 0)), rate
    
//printfn "Earned %s at %s per minute" (prettifyE resetModPoints) (prettifyE (secondsThisLoop / 60.0))
[
    dumpRateSummary "Reset" resetModPoints (secondsThisLoop / 60.0)
    dumpRateSummary $"Stay({nextLoopCause.Title})" nextLoopModPoints value.TimeNeededMinutes
]
|> List.sortByDescending snd
|> List.map fst
|> dumpAction
//printfn "To earn %s at %s per minute" (prettifyE nextLoopModPoints) (prettifyE nextLoopValueEstimate)