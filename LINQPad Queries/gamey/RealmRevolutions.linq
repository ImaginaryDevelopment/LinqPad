<Query Kind="FSharpProgram">
  <Namespace>Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols</Namespace>
</Query>

[<Measure>] type Bp
[<Measure>] type F // Food
[<Measure>] type G // Gold
[<Measure>] type min
[<Measure>] type hr
[<Measure>] type day

let bpOwned = 1.54e17<Bp>
let bpRate =  5.69e15<Bp/s> // 1.51e12<Bp/s> // 2.17e11<Bp> / 1.0<s>
// 37 cows from next homestead, homestead 9, farm 12, well 15
// 5.34e7<F> / 1.0<s> 
let foodRate =  4.93e7<F/s>
let foodOwned = 0.00e1<F>
let gRate = 1.16e32<G/s> // 4.73e31<G/s> // 1.95e27<G/s> //3.80e26<G> / 1.0<s>
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
            i,x,float c |> sci , t , ttt
        )
        
let inline displayIteration<[<Measure>]'T> (i,x,c,t:float<'T>, ttt) = (i,x,c,float t |> sci, formatTime ttt)

    
module Dru =
    let i,nextCost = 58, 2.88e18<Bp>

    let costs = 
        iterateToTarget "dru" i 65 nextCost bpRate bpOwned
        
module Elf =
    let runCost name i nextCost target = name,iterateToTarget name i target nextCost bpRate bpOwned
    let mudCube = runCost "elfMud" 44 5.93e17<Bp> 48
    let forestEle = runCost "elfEle" 18 1.32e11<Bp> 49
    let devilFly = runCost "elfDev" 14 1.24e11<Bp> 39
    let devilSpid = runCost "elfSpid" 16 7.46e12<Bp> 29
    
    let cost = 
        let individual = 
            [   //mudCube
                forestEle
                devilFly
                devilSpid
            ]
            |> List.map (fun (n,x) -> 
                n,
                    Seq.last x 
                    |> fun (_,_,_,c,t) -> (c,t)
            )
        let total = 
            individual 
            |> Seq.map snd 
            |> Seq.fold(fun (x,t) (x', t') ->
                x' + x, t' + t
                ) (LanguagePrimitives.GenericZero, LanguagePrimitives.GenericZero)
            |> fun (x,t) -> float x |> sci, formatTime t
        total, (individual |> List.map(fun (n,(x,t)) -> n, float x |> sci,formatTime t))
        //|> (fun (x,t) -> float x |> sci, formatTime t)
        
module Gold =     
    module Gar =
        let i, nextCost = 30, 1.07e33<G>
        let nextUpgrade = if i % 10 = 0 then i + 10 else int (Math.Ceiling(float i / 10.0) * 10.0)
        let costs =
            iterateToTarget "gar" i nextUpgrade nextCost gRate gOwned
    module Thrones =
        let i, nextCost = 0, 3.17e35<G>
        let costs = iterateToTarget "thr" i 1 nextCost gRate gOwned
(Gold.Thrones.costs |> List.map displayIteration).Dump("thrones")        
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


(Dru.costs |> List.map displayIteration).Dump("To Druid Assim 1<Bp>")     
(Elf.cost).Dump("To Elf 4 1<Bp>")
(Gold.Gar.costs |> List.map displayIteration).Dump(sprintf "Garrisons<G>")  
//
(Food.HomesteadCosts |> List.map displayIteration).Dump("Homesteads<G>")
(Food.WellCosts |> List.map displayIteration).Dump("Wells<F>")