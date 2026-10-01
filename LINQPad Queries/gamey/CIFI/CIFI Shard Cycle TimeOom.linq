<Query Kind="FSharpExpression" />

// CIFI: how many minutes per Operation (Shards)
let thousand, million, billion, trillion, quadrillian, quintillion, sextillian = 1e3, 1e6, 1e9, 1e12,1e15, 1e18, 1e21
let septillian,octillion = 1e24,1e27 // https://en.wikipedia.org/wiki/Names_of_large_numbers
let goal1 = 5.0 * 1e75
let goal2 = 196.0 * septillian
let saved = 3.21e73 // * septillian
let tickSpeed = 5.5
// each shard phase has ticks needed to complete
let ticksPerOp = 6 + 8 + 16 + 8 + 12 + 5 + 10 // prepare next
let income = 1.89e74

let displayTime seconds =
    (Ok (seconds,"seconds"),[
    
    // seconds in a minute
    60.0, "minutes"
    // minutes in an hour
    60.0, "hours"
    // hours in a day
    24.0, "days"
    ])
    ||> Seq.fold(fun current (nextUnits,nextDisplay) ->
        match current with
        | Error (i,display) -> Error (i,display)
        | Ok (i, oldDisplay) ->
            if i > nextUnits then
                Ok(i / nextUnits,nextDisplay)
            else Error (i, oldDisplay)
    )
    |> function
        | Ok (i, disp)
        | Error(i,disp) -> sprintf "%.1f%s" i disp
    
let calcGoalSeconds goal = float (goal - saved) / ( income / tickSpeed)
let secondsPerOp = float ticksPerOp * tickSpeed
calcGoalSeconds goal1 |> displayTime, calcGoalSeconds goal2 |> displayTime, secondsPerOp / 60.0 |> sprintf "%.1f minutes per op"