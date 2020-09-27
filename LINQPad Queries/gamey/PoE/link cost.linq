<Query Kind="FSharpProgram">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <Namespace>HtmlAgilityPack</Namespace>
</Query>

// calculate margin on high ticket items unlinked vs 5/6-Linked
// https://github.com/5k-mirrors/misc-poe-tools/blob/master/doc/poe-ninja-api.md
// https://poe.ninja/api/data/currencyoverview?league=Heist&type=Fragment
type [<Measure>] Fusing
type [<Measure>] Chaos
type [<Measure>] Exalted
// how many fusings do you get per 1 chaos?
let fusingsPerChaos = 2.6m<Fusing/Chaos>
let linkCostAvg = 1500m<Fusing> // avg (not accounting for quality)
let chaosPerExalted = 80m<Chaos/Exalted>

// item purchase price unlinked
let unlinkedCost = 80m<Chaos>

let getChaosCostOfFusings (x:decimal<Fusing>) =
    //let x: decimal<Fusing> = decimal x * 1m<Fusing>
    x / fusingsPerChaos
    
let toExChMix (x:decimal<Chaos>) : decimal<Exalted>*decimal<Chaos> =
    let ex = x / chaosPerExalted
    let r = x - (ex * 1m<1/Exalted> |> Math.Truncate |> (*) 1m<Exalted> ) * chaosPerExalted
    ex,r

let displayCost title (x: decimal<Chaos>) = 
    printfn "%s Cost: %.2f" title x
    let ex,c = toExChMix x
    printfn "Or %.0f + %.0f" ex c

    
// skipping socketing cost?


let linkTotalCost = getChaosCostOfFusings linkCostAvg
displayCost "Link" linkTotalCost
let total =
    linkTotalCost + unlinkedCost

displayCost "Total Item and Link" total
//printfn "Total Item Cost = %.2f" total
//printfn "Or 
|> Dump
|> ignore

//let exaltedChaosCost =
    

