<Query Kind="FSharpExpression" />

let lifecycle = 40
let caloriesPerLife = 16000
let caloriesPerCycle = caloriesPerLife/lifecycle

let dupes = 13
let dupeNeed = dupes * 1000 // assuming no bottomless

let crittersNeeded =
    // try up to x critters
    [1..60]
    |> List.map(fun i -> 
        let c = i * caloriesPerCycle 
        c>dupeNeed,(i,c)
    )
    |> List.tryFind fst
    |> Option.map snd
crittersNeeded