<Query Kind="FSharpProgram" />

type [<Measure>] Alt
type [<Measure>] J
type [<Measure>] F
type [<Measure>] Chaos


let toF = 1.0<F> / 4.0<J>
let toJ = 1.0<J> / 2.0<Alt>

let altCost = 1.0<Alt> / 0.0833<Chaos>
let jewelerCost = 1.0<J> / 0.1<Chaos>
let fusingCost = 1.0<F> / 0.41<Chaos>

let fPerCDisplay (x:float<F/Chaos>) = sprintf "%f fusing(s) per chaos" x
// 10 chaos => 120 alt => 60J => 12 fusing
// 10 chaos => 10J => 2.5 fusing
let values =
    [
        "Alteration value", altCost * toJ * toF
        "jeweler value", jewelerCost * toF
        "fusing value", fusingCost
    ]
printfn "higher is better fusing per chaos"
values
|> List.sortBy (snd>>((*) -1.0))
|> List.map(fun (x,y) -> x, fPerCDisplay y)
|> fun x -> x
|> Dump
|> ignore