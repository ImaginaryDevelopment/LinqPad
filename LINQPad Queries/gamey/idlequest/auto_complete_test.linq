<Query Kind="FSharpProgram" />

// Temporary: probe how many Util.ReadLine suggestions LINQPad autocomplete will surface.
// Result: autocomplete lists stop at 9999 suggestions (indices "0".."9998").
// item-lookup.linq uses maxUtilReadLineSuggestions = 9999 from this measurement.

open System

let count = 30_000
let suggestions =
  Array.init count (fun i -> string i)

printfn "Prepared %d suggestions (\"0\" .. \"%d\")." count (count - 1)
printfn "Start typing a number in the prompt; see how far autocomplete lists go."

let picked = Util.ReadLine("Type to test autocomplete (blank exits)", "", suggestions)
printfn "You entered: %A" picked
