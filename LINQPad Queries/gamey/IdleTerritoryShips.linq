<Query Kind="FSharpProgram" />

let shipyards = 8
let shipsNeeded = 550

module Helpers =
    /// Calls ToString on the given object, passing in a format-string value.
    let inline stringf format (x : ^a) = 
        (^a : (member ToString : string -> string) (x, format))
open Helpers

module Formatting = // https://cseducators.stackexchange.com/questions/4425/should-i-teach-that-1-kb-1024-bytes-or-1000-bytes/4426
    let inline numberFormat x = stringf "N1" x
    let suffix = [ "";"K";"M";"B";"T";"e15";"e18"]
    let format (* multiplier *) (x:uint64) =
        let multiplier = 1000uL
    
        let formatValue i v = sprintf "%s%s(%s)" (numberFormat v) suffix.[i] (v.ToString("G2"))
        let x' = double x
        [0..suffix.Length - 1]
        |> Seq.choose(fun i -> 
            let p:uint64 = pown multiplier i 
            let v = double x' / double p
            let result =
                if v >= 1.0 then
                    sprintf "%s%s(%s)" (numberFormat  v) suffix.[i] (x.ToString "G2")
                    |> Some
                elif i = 0 then
                    formatValue i x'
                    |> Some
                else None
            result
        )
        |> Seq.rev
        |> Seq.head
        
open Formatting

let getWorkerCost baseCost (i:int) =
    if i = 0 then failwith "there is no worker zero"
    Math.Round(float baseCost * Math.Pow(1.15, float (i - 1)), MidpointRounding.AwayFromZero) |> uint64
let getPerShipyard () =
    let needed = Math.Round(double shipsNeeded / double shipyards, MidpointRounding.AwayFromZero) |> int
    [1..needed]
    |> Seq.map (getWorkerCost 40_000)
    |> Seq.mapi(fun i v -> i + 1, format v, v)
    |> List.ofSeq
    |> fun x ->
        let totalCost = x |> List.sumBy(fun (_,_,v) -> v)
        needed,totalCost,format totalCost //, totalCost.ToString("C")
getPerShipyard()
|> fun (num,tc, f) ->
    sprintf "%i ships x %i shipyards => %s + %s wood needed" num shipyards (40_000 * shipyards |> uint64 |> format) (tc * uint64 shipyards |> format)
|> Dump
|> ignore
