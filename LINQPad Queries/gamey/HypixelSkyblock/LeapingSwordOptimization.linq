<Query Kind="FSharpProgram" />

// run simulations on fastest way to get leaping
let minionLimit = 6
let target = 245760

[<Measure>] type sec
[<Measure>] type m
[<Measure>] type hr
[<Measure>] type day

let step = 10m * 1m<hr>
    

let sToM= 1m<m>/60m<sec>
let mToHr = 1m<hr>/60m<m>
let hrToDay = 1m<day>/24m<hr>

let limit = 200m<day> / hrToDay
let intContainer title =
    let dc = new DumpContainer()
    dc.Content <- 0
    dc.Dump()
    fun (x:int<_>) ->
        dc.Content <- sprintf "%s: %i" title x
        
let incContainer title =
    let dc = new DumpContainer()
    let mutable value = 0
    dc.Content <- value
    dc.Dump()
    fun x ->
        value <- value + x
        dc.Content <- sprintf "%s: %i" title value
let dcCount = intContainer "Count"
let dcStates = intContainer "States"
let dcFin = incContainer "Fin"
// simulate leaving it alone, leveling a minion, and buying another minion
// simulations could use a shrinker

// minion time, eye cost
type Minion = {Cooldown:decimal<sec>;Level:int;Cost:decimal}
let getMinionByLevel x =
    {Cooldown=fst rates.[x - 1];Level=x;Cost=snd rates.[x - 1]}
// how long does it take this change to pay for itself
let getPayoff changeCost (rate:decimal) = changeCost / rate
    
let hasUpgrade lvl =
    let next = lvl + 1
    if rates.Length > next then
        true
    else false
    
// division by 2 for producing block/monster, then harvesting minions
let getHrProduction lvl :decimal<1/hr>=
    let cd = (getMinionByLevel lvl).Cooldown
    let perHr = cd * sToM * mToHr
    
    1m / perHr / 2.0m
    
let getDayProduction x:decimal<1/day>=
    let hr = getHrProduction x
    let day = hr / hrToDay
    day
    
getHrProduction 1
|> Dump
|> ignore

let produce (step:decimal<hr>) x : decimal =
    if List.length x = 0 then failwithf "production requires minions"
    else x |> Seq.sumBy getHrProduction |> (*) step
    

getDayProduction 1
|> Dump
|> (/) (decimal target)
|> sprintf "Level 1 completes in %f days"
|> Dump
|> ignore

let purchase = snd rates.[0]
type SimuState={Hours:decimal<hr>; Inventory:decimal; Record: (string*int) list; Minions: int list} with 
    member x.Days = x.Hours * hrToDay

type FinishedSimu = Completed of SimuState
// number will be zero or positive if target achieved
let getStateDistance x = 
    x.Inventory - decimal target
let getStateHourDistance step x =
    let dist = getStateDistance x
    // if we have already reached, then zero hours to reach goal
    if dist >= 0m then
        0m
    else
        let production = produce step x.Minions
        dist / production
    
let recordDecision past next =
    match past with
    | (d,i) :: rem when d = next -> (d,i+1)::rem
    | x -> (next,0)::x
    
let unfoldUpgrades x : SimuState seq =
        // simulate upgrades
        let upgrade inventory lvl =
            if hasUpgrade lvl then
                let next = lvl + 1
                let m = getMinionByLevel next
                if m.Cost <= inventory then
                    Some(inventory - m.Cost, next)
                else None
            else None
                
        let minionGroups =
            x.Minions
            |> List.distinct
        // get the distinct levels possible to upgrade
        // what if we produce enough to level multiple together?
        // need to simulate upgrading by Level, 
        let states=
            minionGroups
            |> Seq.choose(fun lvl ->
                // yield upgrading one on the level
                upgrade x.Inventory lvl
                |> function
                    |None -> None
                    | Some (inventory,m) ->
                        let i = x.Minions |> Seq.findIndex(fun m -> m = lvl)
                        let minions = x.Minions |> List.mapi(fun i' m' -> if i' = i then m else m')
                        let dec = sprintf "Upgrade from %i" lvl
                        Some {x with Inventory = inventory
                                     Minions= minions
                                     Record = recordDecision x.Record dec
                        }
            )
        states
        
// assumes only purchasing 1 per step
let unfoldPurchases x =
    if x.Minions.Length < minionLimit && x.Inventory >= decimal purchase then
        // distance to goal without buying new
        let stay = getStateHourDistance step x
        let m = 1 :: x.Minions
        // check if there's any chance a purchase would have enough time to pay for itself
        let next = {x with 
                        Inventory = x.Inventory - decimal purchase
                        Record = recordDecision x.Record "Purchase"
                        Minions = m
                    }
        let go = getStateHourDistance step next
        if go > stay then
            [next]
        else List.empty
    else List.empty
     
        //else printfn "Stay was %f, go was %f" stay go
    
// unfolds a single decision point - passing 1 hour
// assumes an unfold every hour
let progressState x: SimuState seq option =
    let produce = produce step
    if getStateDistance x >= 0m then
        printfn "State is finished %A" x
        None
    elif x.Hours + step > limit then
        None
    else
        dcCount (int <| decimal x.Hours)
        seq{
            // yield Stay
            yield {x with Record = recordDecision x.Record "Stay"}
            // yield purchase
            yield! unfoldPurchases x
            yield! unfoldUpgrades x
        }
        |> Seq.map(fun x -> {x with Inventory = x.Inventory + produce x.Minions; Hours = x.Hours + step})
        
        |> Some
        
let unfoldStates : SimuState list -> (FinishedSimu list * SimuState list) option = 
    fun states ->
        dcStates states.Length
        states
        |> Seq.choose progressState
        |> Seq.collect id
        |> List.ofSeq
        |> List.partition(fun ss -> getStateDistance ss >= 0m)
        |> fun (f,states) ->
            dcFin f.Length
            //if f.Length > 0 then
            //    printfn "%i finished, %i continue" f.Length states.Length
            let completes = f |> List.map Completed
            if completes.Length > 0 then
                Some(completes, List.empty)
            elif completes.Length < 1 && states.Length < 1 then
                None
            elif states.Length < 1 then
                Some(completes, List.empty)
            else Some(completes, states)
        
    
// states of various decision trees
[{ Hours=0m<_>; Inventory =0m; Record = List.empty;Minions = [1]}]
|> Seq.unfold unfoldStates
|> Seq.collect id
|> Seq.map(function |Completed x -> x)
|> Seq.minBy(fun x -> x.Hours, -1m * x.Inventory)
|> Dump
|> ignore