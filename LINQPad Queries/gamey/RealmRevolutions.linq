<Query Kind="FSharpProgram">
  <Namespace>Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols</Namespace>
</Query>

[<Measure>] type Bp
[<Measure>] type F // Food
[<Measure>] type G // Gold
[<Measure>] type min
[<Measure>] type hr
[<Measure>] type day
// wagon build https://www.reddit.com/r/realmrevolutions/comments/8u2evy/wagon_run_guide/ / https://steamcommunity.com/sharedfiles/filedetails/?id=1413968805
// assim costs https://realm-revolutions.wikia.com/wiki/Prestige_(Revolution_%26_Assimilation)
let inline fTryOrF (f:'t -> 'tResult) (fCatch:exn -> 'tResult) (arg:'t) =
    try
        f arg
    with ex ->
        fCatch ex

let inline fTry<'t,'tResult> title (relevance:obj) (f:'t -> 'tResult) (arg:'t) = 
    try
        f arg
    with _ ->
        relevance.Dump(description=title)
        reraise()

let fTry' title relevance f = 
    fTry title relevance f ()
        
let bpOwned = 1.08e18<Bp>
let bpRate =  1.62e20<Bp/s>
let foodRate =  6.69e8<F/s>
let foodOwned = 0.00e1<F>
let gRate = 1.75e37<G/s> 
let gOwned = 6.00e29<G>

let dumpt t x = x.Dump(description=t); x
let sToMin (x:float<s>):float<min> = x / 60.0<s/min>
let minToHr (x:float<min>):float<hr> = x / 60.0<min/hr>
let hrToDay (x:float<hr>):float<day> = x / 24.0<hr/day>
let sci = float >> fun x -> x.ToString("0.##e-0")

let inline timeToTarget rate owned target = 
    (target - owned) / rate
let rec formatTime (x:float<s>) = TimeSpan(0,0,int x).ToString()
type Iteration<[<Measure>]'t> = {IterationIndex:int; ItemIndex:int; Cost: float<'t>; TotalCost:float<'t>; TimeToTarget:float<s>}
    with 
        member x.ToDisplay() =
            
            let time = 
                fTryOrF (fun () ->
                    float x.TimeToTarget |> DateTime.Now.AddSeconds |> fun x -> x.ToShortTimeString()
                ) (fun _ -> "out of range")()
                
            {IterationIndex=x.IterationIndex; ItemIndex=x.ItemIndex; Cost= float x.Cost |> sci; Total=float x.TotalCost|> sci;Time = time; Wait= formatTime x.TimeToTarget}

and IterationDisplay = {IterationIndex:int; ItemIndex:int; Cost: string; Total:string; Wait:string;Time:string}
// i = 0 then it is base cost, i > 0 it is next cost
let inline getNCost factor targetI (i,cost:float<'t>) = 
    if i = targetI then
        cost
    else 
        let countDown = i > targetI
        cost * Math.Pow(factor,if countDown then float -i else float i)

 
let inline iterateToTarget'<[<Measure>]'T> name (rate:float<'T/s>) (owned:float<'T>) i targetI (nextCost:float<'T>,factor) =
    let countDown = i > targetI
    if countDown then
        [targetI..i]
    else
        [i+1..targetI]        
     
    |> Seq.mapi(fun i x ->
        i, x, nextCost * Math.Pow(factor,if countDown then float -i else float i)
    )
    |> Seq.scan (fun (items,total) ((i,x,cost)) ->
        let discountOwned = 
            // never take out the amount owned if you are doing a countdown
            if i = 0 && not countDown then 
                owned 
            else
                0.0<_>
                 
        let tc = 
            fTry "tcCalc" (cost,total,"cost,total") (fun () ->
             
                if countDown then LanguagePrimitives.GenericZero else (cost + total - discountOwned)
            ) ()
        let ttt= 
            fTry "tttCalc" (rate,tc,"rate,tc") (fun () ->
                timeToTarget rate LanguagePrimitives.GenericZero tc
            ) ()
        let result = 
                {
                    IterationIndex=i
                    ItemIndex=x
                    Cost=cost
                    TotalCost= tc
                    TimeToTarget= ttt
                }
        //(i,x,cost,cost + total - discountOwned)::items, cost + total - discountOwned
        result::items, cost + total - discountOwned
    ) (List.Empty,0.<_>)
    |> Seq.map fst
    |> Seq.last
    |> List.rev
        )
let inline iterateToTarget<[<Measure>]'T> name (rate:float<'T/s>) (owned:float<'T>) i targetI (baseCost:float<'T>,factor) =
    let countDown = i > targetI
    printfn "Iteration is countdown? %A" countDown
    let nextCost = if i = 0 then baseCost else (getNCost factor targetI (i,baseCost) * LanguagePrimitives.GenericOne)
    if countDown then
        [targetI..i]
    else
        [i+1..targetI]        
    
    |> Seq.mapi(fun j x ->
        let getn= getNCost factor x (j,nextCost)
        let legacy = nextCost * Math.Pow(factor,if countDown then float -j else float j)
        //printfn "Step %i v=%i; (%A,%A)" j x getn legacy
        j, x, legacy //nextCost * Math.Pow(factor,if countDown then float -i else float i)
    )
    |> Seq.scan (fun (items,total) ((i,x,cost)) ->
        let discountOwned = 
            // never take out the amount owned if you are doing a countdown
            if i = 0 && not countDown then 
                owned 
            else
                0.0<_>
                
        let tc = 
            fTry "tcCalc" (cost,total,"cost,total") (fun () ->
            
                if countDown then LanguagePrimitives.GenericZero else (cost + total - discountOwned)
            ) ()
        let ttt= 
            fTry "tttCalc" (rate,tc,"rate,tc") (fun () ->
                timeToTarget rate LanguagePrimitives.GenericZero tc
            ) ()
        let result = 
                {
                    IterationIndex=i
                    ItemIndex=x
                    Cost=cost
                    TotalCost= tc
                    TimeToTarget= ttt
                }
        //(i,x,cost,cost + total - discountOwned)::items, cost + total - discountOwned
        result::items, cost + total - discountOwned
    ) (List.Empty,0.<_>)
    |> Seq.map fst
    |> Seq.last
    |> List.rev
    
// math to countdown to an item's base cost
// not sure this works right
let countDownSample() = // name (rate:float<'T/s>) (owned:float<'T>) i targetI (nextCost:float<'T>,factor) 
    iterateToTarget "turtle" bpRate bpOwned 48 0 (1.48e21<Bp>,2.0)
    |> List.map(fun x -> x.ToDisplay())
    |> dumpt "turtle"
    |> ignore
    ()
    
//let inline iterateToTarget'<[<Measure>]'T> name i targetI (nextCost:float<'T>) (rate:float<'T/s>) (owned:float<'T>) =        
iterateToTarget' "turtle" 48 54 1.48e21<Bp> bpRate bpOwned
|> List.map(fun x -> x.ToDisplay())
|> dumpt "turtle"
|> ignore

module Monsters =
    type ForestMonster = 
        | Mosquito
        | Spider
    type PlainsMonster =
        | Rat
        | Scorpio
        | Lizard
        | Eagle
        | Turtle
        | Griffin
        | RoyalGriffin
        | BattleElephant // 1.79e9
        | MonsterOfPlains // [2]
        
    type Monster = 
        | Forest of ForestMonster
        | Plains of PlainsMonster
    
    let getMonsterBaseCost =
        function
        | Forest Mosquito -> 10.0
        | Forest Spider -> 0.0
        | Plains BattleElephant -> 1.70e9
        | Plains MonsterOfPlains -> 2.56e10
        >> (*) 1.0<Bp>
        
    let getScaleFactor =
        function
        | Forest _ -> 2.0
        | Plains _ -> 2.3
    let getCostScale x =  getMonsterBaseCost x, getScaleFactor x
open Monsters

    
let iterateMonster i targetI x = //name (rate:float<'T/s>) (owned:float<'T>) i targetI (nextCost:float<'T>,factor) 
    let name = sprintf "%A" x
    let mInputs = getCostScale x
    iterateToTarget name bpRate bpOwned i targetI mInputs
    
// should return 2.84e12

Monster.Forest Mosquito
|> iterateMonster 0 80
|> List.map(fun x -> x.ToDisplay())
|> dumpt "Dru 3"
|> ignore


//
//
//
//
//
//
//
//
//
//
//
//
//module Dru =
//    let i,nextCost = 60, 1.15e19<Bp>
//
//    let costs = 
//        iterateToTarget "dru" i 80 nextCost bpRate bpOwned
//        
//module Elf =
//    let runCost name i nextCost target = name,iterateToTarget name i target nextCost bpRate bpOwned
//    let mudCube = runCost "elfMud" 44 5.93e17<Bp> 48
//    let forestEle = runCost "elfEle" 45 1.78e19<Bp> 49
//    let devilFly = runCost "elfDev" 14 1.24e11<Bp> 39
//    let devilSpid = runCost "elfSpid" 16 7.46e12<Bp> 29
//    
//    let cost = 
//        let individual = 
//            [   //mudCube
//                forestEle
//                //devilFly
//                //devilSpid
//            ]
//            |> List.map (fun (n,x) -> 
//                n,
//                    Seq.last x 
//                    |> fun iDisplay -> (iDisplay.Cost,iDisplay.TimeToTarget)
//            )
//        let total = 
//            individual 
//            |> Seq.map snd 
//            |> Seq.fold(fun (x,t) (x', t') ->
//                x' + x, t' + t
//                ) (LanguagePrimitives.GenericZero, LanguagePrimitives.GenericZero)
//            |> fun (x,t) -> float x |> sci, formatTime t
//        total, (individual |> List.map(fun (n,(x,t)) -> n, float x |> sci,formatTime t))
//        //|> (fun (x,t) -> float x |> sci, formatTime t)
//        
//module Gold =
//    module Gar =
//        let i, nextCost = 30, 1.07e33<G>
//        let nextUpgrade = if i % 10 = 0 then i + 10 else int (Math.Ceiling(float i / 10.0) * 10.0)
//        let costs =
//            iterateToTarget "gar" i nextUpgrade nextCost gRate gOwned
//    module Thrones =
//        let i, nextCost = 0, 3.17e35<G>
//        let costs = iterateToTarget "thr" i 1 nextCost gRate gOwned
//        
//(Gold.Thrones.costs |> List.map (fun x -> x.ToDisplay())).Dump("thrones")
//type Building = 
//    | Farm
//    | Well
//    | Stable
//    | MasterHouse
//    | Warehouse
//    | Homestead
//module Food = 
//    let HomesteadCosts =
//        let i, nextCost = 9, 3.53e30<G>
//        iterateToTarget "homestead" i (i+5) nextCost gRate gOwned
//    let WellCosts =
//        let i, nextCost,waterboys = 14, 4.88e10<F>, 3.43e10<F>
//        iterateToTarget "well" i (i+5) nextCost foodRate (waterboys+foodOwned)
////    let FarmCosts = 
////        let i, nextCost, 
//let displayTimeToTarget (x:DateTime) =
//     Math.Round(decimal x.Hour + decimal x.Minute / 60.0m,2)
//
//// Masters 7.36e11 -> 1.01e7% (from 35983)
//(Dru.costs |> List.map (fun x -> x.ToDisplay())).Dump("To Druid Assim<Bp>")
//(Elf.cost).Dump("To Elf 4 1<Bp>")
//(Gold.Gar.costs |> List.map  (fun x -> x.ToDisplay())).Dump(sprintf "Garrisons<G>")  
////
//(Food.HomesteadCosts |> List.map  (fun x -> x.ToDisplay())).Dump("Homesteads<G>")
//(Food.WellCosts |> List.map  (fun x -> x.ToDisplay())).Dump("Wells<F>")