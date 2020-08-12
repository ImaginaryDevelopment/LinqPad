<Query Kind="FSharpProgram" />

[<Measure>] type Plain
[<Measure>] type Enchanted
[<Measure>] type EEye


let conversion = 32*5
module Enchants =
    let toPlain (x:int<Enchanted>):int64<Plain> =
        int64 x
        |> Checked.(*) (int64 conversion)
        |> (*) 1L<Plain>
    let fromPlain(x:int<Plain>):int<Enchanted> =
        int x / conversion * 1<Enchanted>
// not including sugar and mushrooms
module EEye =
    let toED (x:int<EEye>):int<Enchanted>= 
        int x * conversion * 1<Enchanted>
        
    let fromED(x:int<Enchanted>):int<EEye>= 
        int x / conversion * 1<EEye>
    

type ItemTotal = {Plain:int64<Plain>; Enchant:int<Enchanted>}

let inline zero< ^T when ^T : (static member Zero : ^T) > : ^T  = LanguagePrimitives.GenericZero
let dumpt title x = x.Dump(description=title); x
let formatCost (x:obj) = String.Format("{0:n0}",x)
    
let getCostTotals (e:int<Enchanted>) =
    let plainCount = e |> Enchants.toPlain
    {Plain = plainCount ;Enchant = e}
    
// not including mushrooms/sugar
let requirement = 12 * 2 * 64<Enchanted> // 12 * 2 fermented eyes
let itemCost =
    requirement    
    |> getCostTotals
    |> dumpt "Currency Option Totals"

let getPlainTotal {Plain=p;Enchant=e} =
    let eTotal = e |> Enchants.toPlain
    p + eTotal
    
let getMonetaryCost marketCost amountNeeded =
    marketCost * amountNeeded

let auctionECost = 1_000_000 * requirement
let costs = [
    // AH estimated, not literal
    "AH E", auctionECost |> Enchants.toPlain
    "B E", 1_900 * 1<Enchanted> |> Enchants.toPlain
    "B Plain", 16L<Plain>
]
let owned = {Plain=zero;Enchant= zero}

costs
|> Seq.map(fun (txt,cost) ->
    sprintf "%s -> $%s" txt <| formatCost (cost * itemCost.Plain)
)
|> dumpt "cost options"
|> ignore

costs
|> Seq.minBy(fun (_,cost) -> cost)
|> fun (txt,cost) -> 
    let plainNeeded = itemCost.Plain - getPlainTotal owned
    // figure out what a good price is for each buying form
    printfn "Plain Value Cost per: %d, E: %d" cost (int64 conversion * cost)
    plainNeeded * cost
    |> formatCost
    |> sprintf "%s -> $%s" txt
|> dumpt "min cost"
|> ignore