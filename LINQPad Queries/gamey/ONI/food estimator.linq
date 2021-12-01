<Query Kind="FSharpProgram" />

let dupes = 5

type CaloricSource = { Lifecycle:float; CaloriesPer:int}

// domestic
let bogjelly = {Lifecycle = 6.6;CaloriesPer=1840}

let target = bogjelly
let cookOpt = 
    Some <| 
        fun x -> x * 2240. / 1840. 
//let lifecycle = 40
//let caloriesPerLife = 16000
let caloriesPerCycle =
    let cpc = float target.CaloriesPer / target.Lifecycle
    match cookOpt with
    | Some f -> f cpc
    | None -> cpc
    

let dupeNeed = float <| dupes * 1000 // assuming no bottomless

let sourcesNeeded =
    // try up to x sources
    [1..60]
    |> List.map(fun i -> 
        let c = float i * caloriesPerCycle 
        c>dupeNeed,(i,c)
    )
    |> List.tryFind fst
    |> Option.map snd
sourcesNeeded 
|> Dump
|> ignore