<Query Kind="FSharpProgram" />

// CIFI: Orbs per day from hours spent + projected Orbs
// Edit the samples list, then run.
// Format: hours (whole), orbs projected — e.g. [ 720, 457.41; 744, 467.81 ]
// adjust formatOrbs to be your orbs indicator like b for billion, or t for trillion

let formatOrbs = sprintf "%0.2ft"
let samples = [
    720, 457.41
    744, 467.81
    792, 488.67
]

let hoursPerDay = 24.0

let perDay (hoursSpent: int) orbsProjected =
    if hoursSpent <= 0 then failwith "Invalid hours"
    else (orbsProjected / float hoursSpent * hoursPerDay)

let formatRate =
    sprintf "%0.2f/d"
let formatHours = sprintf "%ih"
let rows =
    samples
    |> List.map (fun (hours, orbs) ->
        let rate = perDay hours orbs
        formatHours hours, formatRate rate, formatOrbs orbs
    )


rows.Dump("Per sample (hours, orbs/day)")

