<Query Kind="FSharpProgram">
  <Namespace>Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols</Namespace>
</Query>

[<Measure>] type Bp
[<Measure>] type F // Food
[<Measure>] type G // Gold
[<Measure>] type min
[<Measure>] type hr
[<Measure>] type day

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
        
let bpOwned = 5.08e17<Bp>
let bpRate =  4.25e14<Bp/s> // 1.51e12<Bp/s> // 2.17e11<Bp> / 1.0<s>
// 37 cows from next homestead, homestead 9, farm 12, well 15
// 5.34e7<F> / 1.0<s> 
let foodRate =  6.69e8<F/s>
let foodOwned = 0.00e1<F>
let gRate = 1.75e37<G/s> //1.16e32<G/s> // 4.73e31<G/s> // 1.95e27<G/s> //3.80e26<G> / 1.0<s>
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

let inline iterateToTarget<[<Measure>]'T> name i targetI (nextCost:float<'T>) (rate:float<'T/s>) (owned:float<'T>) =
    let countDown = i > targetI
    if countDown then
        [targetI..i]
    else
        [i+1..targetI]        
    
    |> Seq.mapi(fun i x ->
        i, x, nextCost * Math.Pow(2.0,if countDown then float -i else float i)
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
        
module Monsters =
    // math to countdown to an item's base cost
    let countDownSample() = 
        iterateToTarget "mosq" 10 0 10240.<Bp> bpRate bpOwned
        |> List.map(fun x -> x.ToDisplay())
        |> dumpt "mosq"
        |> ignore
        ()
    iterateToTarget "cobra" 47 50 3.16e17<Bp> bpRate bpOwned
    |> List.map (fun x -> x.ToDisplay())
    |> dumpt "cobra"
    |> ignore
    type Monster = 
        | Mosquito
        | Spider
        
    let getMonsterBaseCost =
        function
        | Mosquito -> 1<Bp>
        | Spider -> 0<Bp>
    
module Dru =
    let i,nextCost = 60, 1.15e19<Bp>

    let costs = 
        iterateToTarget "dru" i 80 nextCost bpRate bpOwned
        
module Elf =
    let runCost name i nextCost target = name,iterateToTarget name i target nextCost bpRate bpOwned
    let mudCube = runCost "elfMud" 44 5.93e17<Bp> 48
    let forestEle = runCost "elfEle" 45 1.78e19<Bp> 49
    let devilFly = runCost "elfDev" 14 1.24e11<Bp> 39
    let devilSpid = runCost "elfSpid" 16 7.46e12<Bp> 29
    
    let cost = 
        let individual = 
            [   //mudCube
                forestEle
                //devilFly
                //devilSpid
            ]
            |> List.map (fun (n,x) -> 
                n,
                    Seq.last x 
                    |> fun iDisplay -> (iDisplay.Cost,iDisplay.TimeToTarget)
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
(Gold.Thrones.costs |> List.map (fun x -> x.ToDisplay())).Dump("thrones")        
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

// Masters 7.36e11 -> 1.01e7% (from 35983)
(Dru.costs |> List.map (fun x -> x.ToDisplay())).Dump("To Druid Assim<Bp>")
(Elf.cost).Dump("To Elf 4 1<Bp>")
(Gold.Gar.costs |> List.map  (fun x -> x.ToDisplay())).Dump(sprintf "Garrisons<G>")  
//
(Food.HomesteadCosts |> List.map  (fun x -> x.ToDisplay())).Dump("Homesteads<G>")
(Food.WellCosts |> List.map  (fun x -> x.ToDisplay())).Dump("Wells<F>")