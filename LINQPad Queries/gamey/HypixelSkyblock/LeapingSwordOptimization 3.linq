<Query Kind="FSharpProgram" />

// simulate all hours
// at each hour step - check for ability to purchase, check for ability to upgrade, branch on all (subtracting from inventory)
// produce for each branch
// select branches that match best target time (accounting for loss of inventory, and production for that hour)


let target = 245760 // 132829
let minionLimit = 4

[<Measure>] type sec
[<Measure>] type m
[<Measure>] type hr
[<Measure>] type day

let step:decimal<hr>= 1m<hr>

type MinionStage =
    {Delay:decimal<sec>;Cost:decimal;Tier:byte}

let rates =
    [
        {Delay=26m<sec>;Cost=80m;Tier=1uy}
        // lvl 3
        {Delay=24m<sec>;Cost=160m + 320m;Tier=3uy}
        // lvl 5
        {Delay=22m<sec>;Cost=512m + 1280m;Tier=5uy}
        {Delay=20m<sec>;Cost=3840m + 10240m;Tier=7uy}
        {Delay=17m<sec>;Cost=20480m + 40960m;Tier=9uy}
        //17m<sec>,81920m
    ]
let purchase = rates.[0].Cost

let getMaxUpgrade inventory =
    rates
    |> Seq.mapi(fun i x ->
        i,x.Cost
    )
    |> Seq.filter(snd >> fun cost -> cost <= inventory)
    |> Seq.map fst
    |> Seq.sortDescending
    |> Seq.tryHead
    
let fin =
    let dc = new DumpContainer()
    dc.Dump("final answer")
    printfn "Limit:%i;Step<hr>:%f" minionLimit step
    fun (x:obj) ->
        dc.Content<- x
    
let upgrade1 inventory minionIndex =
    let nextI = minionIndex + 1
    if nextI < rates.Length then
        let cost = rates.[nextI].Cost
        if cost <= inventory then
            Some nextI
        else None
    else None
    
let sToM = 1m<m>/60m<sec>
let mToHr = 1m<hr>/60m<m>
let hrToDay = 1m<day>/24m<hr>

    
let getHrProduction delay : decimal<1/hr>=
    let perHr = delay * sToM * mToHr
    1m / perHr / 2.0m
let getHrProductionAll : _ -> decimal<1/hr> =
    List.map (fun m -> rates.[m].Delay |> getHrProduction) >> List.sum
let getProductionByStep (step:decimal<hr>) delays =
    getHrProduction delays
    |> (*) step
    
let getRemaining (inventory:decimal) (rate:decimal<1/hr>) =
    (decimal target - inventory) / rate
    
let produce mins:decimal =
    let productions = mins |> Seq.map(fun i -> rates.[i].Delay |> getProductionByStep step)
    productions |> Seq.sum
    
type Simulation = {
        Inventory:decimal
        TotalCost: decimal
        MinionIndices:int list
        ProducedPerHour:decimal<1/hr>
        WhenObtained:decimal<hr>
        TotalTimeToTarget:decimal<hr>}
type UpgradeState = {SimuIndex:int;MinionIndex:int;CanUpgrade:bool}

let getTotalTimeToTarget hoursPassed inventory minionIndices = 
    let production = produce minionIndices 
    (decimal target - inventory) / (production * 1m<1/hr>) + hoursPassed

    
let getUpgradeCombos hour {Inventory=inventory; MinionIndices=minions;TotalCost=tc}: Simulation list=
    let n_choose_k n k =
        let rec choose lo  =
            function
          |0 -> [[]]
          |i -> [for j=lo to (Array.length n)-1 do
                      for ks in choose (j+1) (i-1) do
                        yield n.[j] :: ks ]
        in choose 0 k            
    match getMaxUpgrade inventory with
    | None -> List.empty
    | Some maxUpgrade ->
        let distinctMinionUpgrades =
            minions
            |> List.mapi(fun i mi -> {SimuIndex=i;MinionIndex=mi;CanUpgrade = mi < maxUpgrade})
            // this eliminates the possibility of upgrading 2 of the same index, not the right place for distinct
            //|> List.distinctBy(fun x -> x.MinionIndex)
        let upgradables = 
            distinctMinionUpgrades
            |> List.choose(fun us -> if us.CanUpgrade then Some us else None)
            |> Array.ofList
        [1..upgradables.Length]
        |> List.collect(n_choose_k upgradables)
        |> List.choose(fun x ->
            let totalCost = 
                x
                |> List.map(fun s -> rates.[s.MinionIndex].Cost)
                |> List.sum
                
            if totalCost <= inventory then
                Some(totalCost,x)
            else None
        )
        |> List.distinctBy(fun (cost,ul) ->
            cost,ul |> List.map(fun x -> x.MinionIndex)
        )
        |> List.map(fun (cost,upgrades) ->
            //(cost,minions,upgrades).Dump("minions")
            //printfn "Need to do %i upgrades" upgrades.Length
            let nextMinions =
                minions
                |> List.mapi(fun i m ->
                    match upgrades |> Seq.tryFind(fun upgr -> upgr.SimuIndex = i && upgr.MinionIndex = m) with
                    | Some _ ->
                        //printfn "Upgrading %A" upgrade
                        m + 1
                    | None ->
                        //printfn "No Upgrade found for %A" (i,m)
                        m
                )
            let nextInventory = inventory - cost
            {   Inventory = nextInventory
                WhenObtained = hour
                TotalCost = tc + cost
                MinionIndices=nextMinions
                ProducedPerHour=nextMinions |> List.map (fun m -> rates.[m].Delay |> getHrProduction) |> List.sum
                TotalTimeToTarget=getTotalTimeToTarget hour nextInventory nextMinions }
        
        )
let branchUpgrades hour x : Simulation list=
    let upgrades = getUpgradeCombos hour x
    upgrades
        


// at each hour step - check for ability to purchase, check for ability to upgrade, branch on all (subtracting from inventory)
// account for loss of inventory, and production for that hour
let simulateHour startHour (x:Simulation) : Simulation list =
    let results =
        [
           // simulate stay
           yield x
           // simulate purchase
           if x.Inventory >= purchase && x.MinionIndices.Length < minionLimit then
               let nextInventory,nextMinion= x.Inventory - purchase, 0::x.MinionIndices
               yield {  Inventory = nextInventory
                        MinionIndices=nextMinion
                        TotalCost = x.TotalCost + purchase
                        WhenObtained=startHour
                        ProducedPerHour= getHrProductionAll nextMinion
                        TotalTimeToTarget=getTotalTimeToTarget startHour nextInventory nextMinion}
           // simulate upgrade
           // only simulating first minion upgrades atm
           yield! branchUpgrades startHour x
        ]
    results
    |> List.map(fun x -> {x with Inventory=x.Inventory + produce x.MinionIndices})

let simulateHourAll startHour (x:Simulation list) =
    x
    |> List.collect (simulateHour startHour)

// assumes that ones that seem slower for a turn won't catch up
// Best time and which simulations reached that time
type SimuState = {HoursPassed:decimal<hr>;BestTime:decimal<hr>;Sims: Simulation list}

// select branches that match best target time 
let pickBest (x:Simulation list) =
    let best =
        x
        |> List.map(fun x -> x.TotalTimeToTarget)
        |> List.min
    best,x
    // use int to do fuzzy matching of ones that are nearly equal
    |> List.filter(fun x -> x.TotalTimeToTarget |> decimal |> int  = (best |> decimal |> int))


// move hour by hour, stopping only when one branch reaches the end
let initial =
    let mi = [2;2;1]
    let sim = { Inventory=40960m
                WhenObtained=0m<hr>
                TotalCost=mi |> List.map(fun i ->  rates.[i].Cost) |> List.sum
                MinionIndices= mi
                TotalTimeToTarget = getTotalTimeToTarget 0m<hr> 0m mi
                ProducedPerHour = getHrProductionAll mi
            }
    {HoursPassed=0m<hr>; BestTime=sim.TotalTimeToTarget; Sims=[sim]}
printfn "Starting from %A" initial    
initial
|> Seq.unfold(fun ss ->
    if ss.BestTime <= ss.HoursPassed || ss.Sims|> List.exists(fun s -> s.Inventory >= decimal target) then None
    else
        let best,hourResult =
            ss.Sims |> simulateHourAll ss.HoursPassed
            |> pickBest
        let ss = {HoursPassed=ss.HoursPassed + step;BestTime=best;Sims=hourResult |> List.sortByDescending (fun s -> s.Inventory)}
        Some(ss,ss)
)
|> Seq.mapi(fun i x ->
    //if i % 50 = 0 then
    if i < 10 then
        x.Dump(sprintf "at %i" i)
    x
)
|> Seq.last
|> fun x ->
    sprintf "%f days" (x.BestTime * hrToDay), x
|> fin