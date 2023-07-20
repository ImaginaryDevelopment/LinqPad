<Query Kind="FSharpProgram" />

// reddit: https://www.reddit.com/r/TerritoryIdle/
// kong forums: https://www.kongregate.com/forums/1023281-territory-idle

// pantheon: https://territoryidle.fandom.com/wiki/Pantheon
// https://qnnit.com/territory-idle-pantheons-and-religions/

// strategies:
// https://gameplay.tips/guides/3994-territory-idle.html
// https://www.reddit.com/r/TerritoryIdle/comments/de3f7s/18_tiles_to_freedom_tips_for_the_beginner/
// https://www.kongregate.com/forums/1023281-territory-idle/topics/1809240-newbie-guide-or-walkthrough

// someone's sheet:
// https://github.com/blackreign/Territory_idle

// wiki https://territoryidle.fandom.com/wiki/Territory_Idle_Wiki

type [<Measure>] Worker
type [<Measure>] Farmer
type [<Measure>] Logger
type [<Measure>] Monk

// 5 squares
type Square =
    | Field
    | Farm
    | Forest
    | Quarry
    | Temple
    
type Pantheon =
    | OpenSky
    | ForestLegend
    | Nature
    | CromLechis
    | FatChickens
    | Handwashing
    | Settlements
    | Guru
    | War
    | Sacred
    | Justice
    
type ReligionBonus =
    | ForestMagic of int
    
let isField = 
    function
    | Field -> true
    | _ -> false
    
let isForest =
    function
    | Forest -> true
    | _ -> false
    
module Maths =
      
    let rec comb n l = 
        match n, l with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs
        
let useXIfIsY y x v =
    if v = y then Some x else None
    
let calculateWheat tiles hasFatChickens (workersPerField:int<Worker>) (farmersPerFarm:int<Farmer>) =
    let farmers = tiles |> List.choose(useXIfIsY Farm farmersPerFarm) |> List.sum
    let workers = tiles |> List.choose(function | Field -> Some workersPerField | _ -> None) |> List.sum
    let farmersAdd = int farmers * if hasFatChickens then 2 else 1
    int workers * (1 + farmersAdd)
    
let calculateFaith tiles pantheons religionBonuses monksPerTemple =
    let hasSacredPath = pantheons |> List.exists(function | Sacred -> true | _ -> false)
    let hasNature = pantheons |> List.exists(function | Nature -> true | _ -> false)
    let monks = tiles |> List.choose(function | Temple -> Some monksPerTemple | _ -> None) |> List.sum
    let forests = tiles |> List.filter isForest |> List.length
    let natureAdd = 
        let ph = if hasNature then 3 * forests else 0 // nature pantheon/heritage
        let rel = religionBonuses |> List.choose(function | ForestMagic i -> Some i | _ -> None) |> List.tryHead |> Option.defaultValue 0
        ph + rel
        
    
    let sacAdd = if hasSacredPath then 5 else 0
    let sacMult = if hasSacredPath then 1.25 else 1.0
    
    int monks * (1 + natureAdd + sacAdd)
    |> float
    |> ( * ) sacMult
    
// assumes 1 is first worker, not 0    
let getWorkerCost baseCost (i:int) =
    if i = 0 then failwith "there is no worker zero"
    Math.Round(float baseCost * Math.Pow(1.15, float (i - 1)), MidpointRounding.AwayFromZero) |> int
    
let optimizeWheatSquares squares hasFatChickens workers farmers =
    if squares < 2 then
        failwithf "Bad squares %i" squares
    let combos =
        [Field;Farm]
        |> List.replicate (squares - 1)
        |> List.collect id
        |> Maths.comb (squares - 1)
        |> List.map(List.sort)
        |> List.distinct
        |> List.sort
        // at least 1 square must be a field to generate wheat
        |> List.map(fun combo -> Field::combo)
    combos
    |> List.map(fun combo ->
        calculateWheat combo hasFatChickens workers farmers,combo

    )
    |> List.sortByDescending fst
    
let optimizeTempleSquares squares pantheons religionBonuses monks =
    if squares < 2 then
        failwithf "Bad squares %i" squares
    let combos =
        [Forest;Temple]
        |> List.replicate (squares)
        |> List.collect id
        |> Maths.comb (squares - 1)
        |> List.map(List.sort)
        |> List.distinct
        |> List.sort
        // at least 1 square must be a field to generate wheat
        |> List.map(fun combo -> Temple::combo)
    combos
    |> List.map(fun combo ->
        calculateFaith combo pantheons religionBonuses monks, combo
    )
    |> List.sortByDescending fst

// 6/3 -> 17500, 22500
// 4/5 -> 7837, 45000
let wheatTiles = 4
let faithTiles = 5
let pantheons = [Pantheon.Sacred; Pantheon.CromLechis]
let relBonuses = [ ReligionBonus.ForestMagic 5 ]
let workers = 96<Worker>
let farmers = 50<Farmer>
let monks = 1000<Monk>

let wheatCombos = 
    let hasFatChickens = pantheons |> List.exists(function | FatChickens -> true | _ -> false)
    optimizeWheatSquares wheatTiles hasFatChickens workers farmers
    // add amber bonus
    |> List.map (fun (v,c) -> float v * 1.25 |> int, c)
    
let faithCombos =
    optimizeTempleSquares faithTiles pantheons relBonuses monks
    
printfn "Wheat"

wheatCombos 
|> List.map(fun (v,c) -> v, c |> List.map string |> List.countBy id)
//allCombinations [Field;Farm]
|> Dump
|> ignore
printfn "Faith"
faithCombos
|> List.map(fun (v,c) -> v, c |> List.map string |> List.countBy id)
|> Dump
|> ignore
