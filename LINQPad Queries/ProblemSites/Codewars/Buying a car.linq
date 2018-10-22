<Query Kind="FSharpProgram" />

open System
[<Measure>] type Percentage
[<Measure>] type Month
type Currentvalue = {OldValue:float;NewPrice:float;Loss:float<Percentage/Month>;Months:int<Month>;Saved:float;Leftovers:float}

let percentage x =
    if x > 1.0 || x < 0.0 then failwithf "bad percentage %f" x
    x * 1.0<Percentage>
    
let depreciate loss value =
    let result =  
        value * (1.<Percentage/Month> - loss)
        |> (*) 1.<Month/Percentage>
//    (loss,value,result).Dump("depreciated")
    result
let calcMonth savingPerMonth x = 
    let month = x.Months + 1<_>
    let loss = if month % 2<Month> = 0<Month> && month > 0<_> then x.Loss + 0.005<Percentage/Month> else x.Loss
    let lossCalcValue = loss // x.Loss
    let result = {  OldValue = depreciate lossCalcValue x.OldValue
                    NewPrice = depreciate lossCalcValue x.NewPrice
                    Months= month
                    Loss = loss
                    Saved=x.Saved + savingPerMonth
                    Leftovers=x.Leftovers
                }
    let result = {result with Leftovers = Math.Round(result.Saved + result.OldValue - result.NewPrice)}
    result
    
let unfolder savingperMonth x =
    if x.Saved + x.OldValue >= x.NewPrice then
        None
    else
        let result = calcMonth savingperMonth x
        Some (result,result)
let nbMonths (startPriceOld: float) (startPriceNew: float) (savingperMonth: float) (percentLossByMonth: float) =
    let percentLossByMonth = percentLossByMonth * 0.01 |> percentage 
    let constructed = {OldValue = startPriceOld; NewPrice=startPriceNew; Loss= percentLossByMonth * 1.<_>;Months=0<_>;Saved=0.;Leftovers=startPriceOld - startPriceNew}
    printfn "Starting on %A" constructed
    if startPriceOld >= startPriceNew then
        [constructed] :> seq<_>
    else
        constructed 
        |> Seq.unfold (unfolder savingperMonth)
        |> Seq.append [constructed]
    |> Seq.map(fun x -> int x.Months, int x.Leftovers)
    |> Seq.last
        
[   2000.0,8000.0,1000.0,1.5
    18000.0,32000.0,1500.0, 1.25
]
|> List.map(fun (a,b,c,d) -> nbMonths a b c d)
|> Dump
|> ignore