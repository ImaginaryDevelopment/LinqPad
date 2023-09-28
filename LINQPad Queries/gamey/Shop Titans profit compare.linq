<Query Kind="FSharpProgram" />

type Result = {
    Rank:int
    Name:string
    Price: int
    SurchargeCost: int
    DiscountBoost: int
    //ProfitPerEnergy: float
    //LossPerEnergy: float
} with
    member x.ProfitPerEnergy = float x.Price * -1. / float x.SurchargeCost
    member x.LossPerEnergy = float x.Price * -0.5 / float x.DiscountBoost
    member x.PLRatio = x.ProfitPerEnergy / x.LossPerEnergy
let emptyResult (name, p, sur, disc) =
    { Name = name; Price = p; SurchargeCost = sur; DiscountBoost = disc; Rank = 0}
let values =
    [
        "Stealth Knife", 1_100, -45, 20
        "Noble Chain", 10_000, -165, 75 // also Silver Thistle, Tomahawk
        "Noble Ring", 14_000, -195, 90
        "XL Magic Potion", 17_000, -190, 85
        "Skull Crusher", 21_000, -240, 110
        "Phoenix Tonic", 31_500, -290, 130
        "Wizard Staff", 32_500, -295, 135
    ]
    |> List.map emptyResult

let byProfit =
    values
    |> List.sortByDescending(fun x -> x.ProfitPerEnergy)
    
let byLoss =
    values
    |> List.sortByDescending(fun x -> x.LossPerEnergy)
    
let isConsistent = List.rev byLoss = byProfit
if not isConsistent then
    eprintfn "Warning: there is at least one outlier"
let index items =
    items
    |> List.mapi(fun i x -> {x with Rank = i})
    
byProfit
|> List.rev
|> index
|> List.rev
|> Dump
|> ignore
if not isConsistent then
    byLoss
    |> List.rev
    |> index
    |> fun x -> x.Dump()