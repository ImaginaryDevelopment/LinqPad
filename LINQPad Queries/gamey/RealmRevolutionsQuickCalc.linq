<Query Kind="FSharpProgram">
  <Namespace>Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols</Namespace>
</Query>

[<Measure>] type Bp
[<Measure>] type F // Food
[<Measure>] type G // Gold
[<Measure>] type min
[<Measure>] type hr
[<Measure>] type day

let rate = 1.74e24<Bp/s>
let owned = 1.98e8<Bp>
let target = 2.59e8<Bp>

let dumpt t x = x.Dump(description=t); x

let sToMin (x:float<s>):float<min> = x / 60.0<s/min>
let minToHr (x:float<min>):float<hr> = x / 60.0<min/hr>
let hrToDay (x:float<hr>):float<day> = x / 24.0<hr/day>
let sci = float >> fun x -> x.ToString("0.##e-0")
let inline timeToTarget rate owned target = 
    (target - owned) / rate
let rec formatTime (x:float<s>) = TimeSpan(0,0,int x).ToString()
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
type Iteration<[<Measure>]'t> = {IterationIndex:int; ItemIndex:int; Cost: float<'t>; TotalCost:float<'t>; TimeToTarget:float<s>}
    with 
        member x.ToDisplay() =
            
            let time = 
                fTryOrF (fun () ->
                    float x.TimeToTarget |> DateTime.Now.AddSeconds |> fun x -> x.ToShortTimeString()
                ) (fun _ -> "out of range")()
                
            {IterationIndex=x.IterationIndex; ItemIndex=x.ItemIndex; Cost= float x.Cost |> sci; Total=float x.TotalCost|> sci;Time = time; Wait= formatTime x.TimeToTarget}

and IterationDisplay = {IterationIndex:int; ItemIndex:int; Cost: string; Total:string; Wait:string;Time:string}        
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
iterateToTarget' "mosq" rate owned 82 95 (4.83e25<Bp>,2.0)
|> List.map(fun x -> x.ToDisplay())
|> dumpt "mosq"
|> ignore