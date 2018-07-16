<Query Kind="FSharpProgram">
  <Namespace>Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols</Namespace>
</Query>

[<Measure>] type Bp
[<Measure>] type F // Food
[<Measure>] type G // Gold
[<Measure>] type min
[<Measure>] type hr
[<Measure>] type day
let bpOwned = 1.50e15<Bp>
let bpRate = 1.51e12<Bp/s> // 2.17e11<Bp> / 1.0<s>
// 37 cows from next homestead, homestead 9, farm 12, well 15
// 5.34e7<F> / 1.0<s> 
let foodRate =  4.93e7<F/s>
let foodOwned = 0.00e1<F>
let gRate = 1.95e27<G/s> //3.80e26<G> / 1.0<s>
let gOwned = 6.00e29<G>
let sToMin (x:float<s>):float<min> = x / 60.0<s/min>
let minToHr (x:float<min>):float<hr> = x / 60.0<min/hr>
let hrToDay (x:float<hr>):float<day> = x / 24.0<hr/day>
let sci = float >> fun x -> x.ToString("0.##e-0")
let inline timeToTarget rate owned target = 
    (target - owned) / rate     
let rec formatTime (x:float<s>) = TimeSpan(0,0,int x).ToString()

let inline iterateToTarget<[<Measure>]'T> name i targetI (nextCost:float<'T>) (rate:float<'T/s>) (owned:float<'T>) =
    [i+1..targetI]
        |> Seq.mapi(fun i x ->
            i, x, nextCost * Math.Pow(2.0,float i)
        )
        |> Seq.scan (fun (items,total) ((i,x,cost)) ->
            let discountOwned = 
                if i = 0 then 
                    owned 
                else 
                    0.0<_>
            
            (i,x,cost,cost + total - discountOwned)::items, cost + total - discountOwned
        ) (List.Empty,0.<_>)
        |> Seq.map fst
        |> Seq.last
        |> List.rev
        |> List.map(fun (i,x,c,t) -> 
            let ttt= timeToTarget rate LanguagePrimitives.GenericZero t
            i,x,float c |> sci ,float t |> sci ,formatTime ttt
        )
        
module Dru1 =
    let i,nextCost = 49, 2.81e15<Bp>

    let dru1costs = 
        iterateToTarget "dru" i 50 nextCost bpRate bpOwned

module Gar =
    let i, nextCost = 20, 1.04e30<G>
    let nextUpgrade = if i % 10 = 0 then i + 10 else int (Math.Ceiling(float i / 10.0) * 10.0)
    let Costs =
        iterateToTarget "gar" i nextUpgrade nextCost gRate gOwned
type Building = 
    | Farm
    | Well
    | Stable
    | MasterHouse
    | Warehouse
    | Homestead
module Food = 
    let HomesteadCosts =
        let i, nextCost = 9, 3.53e30<G>
        iterateToTarget "homestead" i (i+5) nextCost gRate gOwned
    let WellCosts =
        let i, nextCost,waterboys = 14, 4.88e10<F>, 3.43e10<F>
        iterateToTarget "well" i (i+5) nextCost foodRate (waterboys+foodOwned)
//    let FarmCosts = 
//        let i, nextCost, 
let displayTimeToTarget (x:DateTime) =
     Math.Round(decimal x.Hour + decimal x.Minute / 60.0m,2)


Dru1.dru1costs.Dump("To Druid Assim 1<Bp>")     
Gar.Costs.Dump(sprintf "Garrisons<G>")  

Food.HomesteadCosts.Dump("Homesteads<G>")
Food.WellCosts.Dump("Wells<F>")