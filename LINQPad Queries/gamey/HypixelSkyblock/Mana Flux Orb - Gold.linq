<Query Kind="FSharpProgram" />

// calculate the fastest path to getting e-fermented eyes (based on getting enough eyes only)

// simulate all hours
// calculate based on existing inventory, and minions, the fastest path to get to X amount of Gold (for making a mana flux orb in this case)

// at each hour step - check for ability to purchase, check for ability to upgrade, branch on all (subtracting from inventory)
// produce for each branch
// select branches that match best target time (accounting for loss of inventory, and production for that hour)

let initialTiers = [ "V"]
let initialInventory = 0m
let target = 1024m * 160m // cost of leaping sword in eyes
let minionProduction = 1m
// how many minions can we dedicate
let minionLimit = 5

[<Measure>] type sec
[<Measure>] type m
[<Measure>] type hr
[<Measure>] type day

// changing the step is untested
let step:decimal<hr>= 1m<hr>

type MinionStage =
    {Delay:decimal<sec>;Produces:decimal;Cost:decimal;Tier:byte}
let rates =
    let p = 1m
    [
        {Delay=22m<sec>;Produces=p;Cost=80m;Tier=1uy}
        // lvl 3
        {Delay=20m<sec>;Produces=p;Cost=160m + 320m;Tier=3uy}
        // lvl 5
        {Delay=18m<sec>;Produces=p;Cost=512m + 1280m;Tier=5uy}
        {Delay=16m<sec>;Produces=p;Cost=3840m + 10_240m;Tier=7uy}
        {Delay=14m<sec>;Produces=p;Cost=20_480m + 40_960m;Tier=9uy}
        //17m<sec>,81920m
        {Delay=11m<sec>;Produces=p;Cost=204_800m+409_600m;Tier=9uy}
    ]
let getMinionsByIndices  =
    List.map(fun i -> rates[i])
let purchase = rates[0].Cost

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
        let cost = rates[nextI].Cost
        if cost <= inventory then
            Some nextI
        else None
    else None
    
let sToM = 1m<m>/60m<sec>
let mToHr = 1m<hr>/60m<m>
let hrToDay = 1m<day>/24m<hr>

    
let getHrProduction (delay,produces:decimal) : decimal<1/hr>=
    let perHr = delay * sToM * mToHr
    1m / perHr / 2.0m * produces
let getMinionHrProduction {Delay=d;Produces=p} = getHrProduction(d,p)
let getHrProductionAll : _ -> decimal<1/hr> =
    List.map getMinionHrProduction >> List.sum
let getProductionByStep (step:decimal<hr>) minion =
    getMinionHrProduction minion
    |> (*) step
    
let produce mins:decimal =
    let productions = mins |> getMinionsByIndices |> List.map(getProductionByStep step)
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
                        yield n[j] :: ks ]
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
                |> List.map(fun s -> rates[s.MinionIndex].Cost)
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
                ProducedPerHour=nextMinions |> List.map (fun m -> rates[m] |> getMinionHrProduction) |> List.sum
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
               let nextInventory,nextMinions= x.Inventory - purchase, 0::x.MinionIndices
               yield {  Inventory = nextInventory
                        MinionIndices=nextMinions
                        TotalCost = x.TotalCost + purchase
                        WhenObtained=startHour
                        ProducedPerHour= nextMinions |> List.map(fun i -> rates[i]) |> getHrProductionAll 
                        TotalTimeToTarget=getTotalTimeToTarget startHour nextInventory nextMinions}
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
    let mi = initialTiers |> List.map(function | "I" -> 0 | "III" -> 1 | "V" -> 2 | "VII" -> 3 | "IX" -> 4 | "XI" -> 5| x -> failwithf "Unknown tier %s" x)
    let minions = mi |> getMinionsByIndices
    let sim = { Inventory=initialInventory
                WhenObtained=0m<hr>
                TotalCost=mi |> List.map(fun i ->  rates[i].Cost) |> List.sum
                MinionIndices= mi
                TotalTimeToTarget = getTotalTimeToTarget 0m<hr> 0m mi
                ProducedPerHour = getHrProductionAll minions
            }
    {HoursPassed=0m<hr>; BestTime=sim.TotalTimeToTarget; Sims=[sim]}
printfn "Starting from %A" initial    
type Display = {    TotalDays:decimal<day>;TotalHours:decimal<hr>;TotalCosts:int;FinalInventory:decimal
                    ConfigurationObtainedAtHour:int;Configuration: MinionStage list}
initial
|> Seq.unfold(fun ss ->
    if ss.BestTime < ss.HoursPassed || ss.Sims|> List.exists(fun s -> s.Inventory >= decimal target) then None
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
    
    x.Sims
    |> List.map(fun s ->
        {   TotalDays = x.BestTime * hrToDay
            TotalHours = x.BestTime
            FinalInventory = s.Inventory
            TotalCosts = int s.TotalCost
            ConfigurationObtainedAtHour = s.WhenObtained * 1m<1/hr> |> int
            Configuration = getMinionsByIndices s.MinionIndices}
    )
    //sprintf "%f days" (x.BestTime * hrToDay), x
|> fin