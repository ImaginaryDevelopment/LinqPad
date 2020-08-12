<Query Kind="FSharpProgram" />

// I don't think we've accounted for the time that has passed being added to best finish on upgrade/stay/purchase paths
// nor do we stop when we hit goal
// maybe this? type Distance = {TimeLeft:decimal<hr>;TimePassed:decimal<hr>}

let target = 245760

[<Measure>] type sec
[<Measure>] type m
[<Measure>] type hr
[<Measure>] type day

let step = 1m<hr>

let rates =
    [
        26m<sec>,80m 
        // lvl 3
        24m<sec>,160m + 320m
        // lvl 5
        22m<sec>,512m + 1280m
        20m<sec>,3840m + 10240m
        17m<sec>,20480m + 40960m
        //17m<sec>,81920m
    ]
    
let sToM= 1m<m>/60m<sec>
let mToHr = 1m<hr>/60m<m>
let hrToDay = 1m<day>/24m<hr>

let getHrProduction delay :decimal<1/hr>=
    let perHr = delay * sToM * mToHr
    1m / perHr / 2.0m
    
let getProduction delays =
    delays
    |> Seq.map getHrProduction 
    |> Seq.sum
    
let getRemaining (inventory:decimal) (rate:decimal<1/hr>) =
    (decimal target - inventory) / rate
    
let produce mins:decimal =
    let productions = mins |> Seq.map(fun i -> fst rates.[i] |> getHrProduction|> decimal)
    productions |> Seq.sum
    
// if purchase finishes faster then Some else None
let tryPurchase (inventory:decimal) delays remainingWithoutPurchase =
    let delays' = fst rates.[0] :: delays
    let rem = getProduction delays' |> getRemaining (inventory - snd rates.[0])
    if rem <= remainingWithoutPurchase then
        Some (inventory,delays')
    else None
    
getRemaining 1m 1000m<1/hr>
|> sprintf "%f hours"
|> Dump
|> ignore

let getTargetTimeToFinish inventory indexes =
    indexes
    |> Seq.map(fun i -> rates.[i] |> fst)
    |> getProduction
    |> getRemaining inventory 
    
let upgradeThings indexes fUpgradeOne =
    [0.. List.length indexes - 1]
    |> Seq.map fUpgradeOne
let findMins (results:list<decimal<hr>* int list>)=    
    let mn =  results |> List.map fst |> List.min
    results |> List.filter(fun (finishesIn,_) -> finishesIn = mn)
    
    
type MinionConfig = {Inventory:decimal; Minions: int list;HoursToObtain:decimal<hr>}    


    
let tryThings minionConfig : decimal<hr>*(MinionConfig list) =
    let inventory,indexes = minionConfig.Inventory, minionConfig.Minions
    let stayRemain = indexes |> getTargetTimeToFinish inventory
    let purchaseMore = 0::indexes |> getTargetTimeToFinish (inventory - snd rates.[0])
    let upgrade tgtIndex =
        // if this one isn't max level
        if tgtIndex + 1 < rates.Length then
            let cost = snd rates.[tgtIndex+1]
            // if this index is same level as previous, don't bother trying upgrade
            if tgtIndex > 0 && indexes.[tgtIndex - 1] = indexes.[tgtIndex] then
                None
            else
                let upgradeFolder (lvlIndex:int,cost:decimal option) (cost':decimal,indexes:int list) =
                    let costOpt = cost |> Option.defaultValue 0m 
                    costOpt + cost', lvlIndex::indexes
                let cost,indexes =
                    let r =
                        indexes
                        |> List.mapi(fun i lvlIndex ->
                            if i = tgtIndex then
                                lvlIndex + 1, Some cost
                            else lvlIndex,None
                        )
                    List.foldBack upgradeFolder r (0m,List.empty)
                Some(indexes,getTargetTimeToFinish (inventory - cost) indexes)
                
                
        else None
    let results =
        [
            yield stayRemain,indexes
            yield purchaseMore, 0::indexes
            let f = upgradeThings indexes upgrade |> Seq.choose id |> Seq.map(fun (x,y) -> y,x)
            yield! f
        ]
    let mn = results |> List.map fst |> List.min
    let winners = results |> List.choose (fun (c,winner) -> if c = mn then Some winner else None) 
    mn, winners |> List.map(fun win -> {Inventory = inventory + produce win;Minions=win})
    
// BestFinish is a list of configurations that appear to tie for the best time thus far
type Status = {BestFinish: decimal<hr>; Configurations: MinionConfig list}
// if there is a time improvement found, keep going
let unfoldState:Status -> (Status option*Status) option =
    fun state ->
        // is this reapplying an hour that already passed?
        let result =
            state.Configurations
            |> List.map (tryThings state.BestFinish)
        let newBest = result |> Seq.head |> fst
        if newBest < state.BestFinish then
            let next = {BestFinish=newBest;Configurations = result |> List.collect snd}
            Some(Some next, next)
        else None
        
let mns = [0]
{BestFinish= getTargetTimeToFinish 0m mns; Configurations= List.singleton {Inventory=0m; Minions =mns}}
//||> tryThings
|> Seq.unfold unfoldState
|> Seq.choose id
//|> Seq.rev
|> Seq.truncate 100
|> Dump
rates
|> Dump
|> ignore