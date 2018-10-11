<Query Kind="FSharpProgram" />

// Calc profit percentage improvement per unit per $

type PurchaseDetail = {CostPerUnit: float; CurrentProfitPercentage: float; PercentageImprovement:float}
// percentage is how much of the overall profits is this one item type contributing
let f (unitsToBuy:int) cost (amountOwned:int) percentage = 
    let costPerUnit= cost / float unitsToBuy
    let percentValuePerUnit= percentage / float amountOwned
    //costPerUnit / float amountOwned * percentage
    {CostPerUnit= costPerUnit; CurrentProfitPercentage= percentage; PercentageImprovement= percentValuePerUnit / costPerUnit}
[
    "Mana Gem", f 6 3.83e70 1062 17.38
    "Tome", f 13 3.35e70 976 2.98
    "Fountain", f 14 4.01e70 841 0.24
    "Rift", f 1 3.20e70 703 60.94
    "Nex", f 1 2.59e70 657 15.20
]
|> Seq.sortBy (snd >> fun x -> x.PercentageImprovement)
|> Seq.rev
|> Dump
|> ignore