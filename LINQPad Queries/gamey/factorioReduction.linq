<Query Kind="FSharpProgram" />


// XXXXXXXXXXXXXXXX -- cancel that - not to include liquids or raw ore
type Item =
    | Science3
    | Battery
    | AdvCircuit
    | Circuit
    | PlasticBar
    | Inserter
    | FastInserter
    | FilterInserter
    | Steel
    | Iron
    | Gear
    | CopperCable
    | Copper
    | SulfuricAcid
type ItemMap = Map<Item,decimal>
let canReduce =
    function
    | Iron -> false
    | Copper -> false
    // Temp
    | SulfuricAcid -> false
    /// Coal and petroleum gas
    | PlasticBar -> false
    | _ -> true
let reduceItem = 
    function
    | CopperCable -> [0.5m,Copper]
    | Gear -> [ 2.0m,Iron]
    | Battery -> [  1.0m,Iron
                    1.0m,Copper
                    2.0m,SulfuricAcid]
    | Science3 -> 
        [   1.0m,Battery
            1.0m,AdvCircuit
            1.0m,FilterInserter
            1.0m,Steel ] 
    | Circuit -> [  1.0m, Iron
                    3.0m, CopperCable]
    | AdvCircuit -> [   2.0m,Circuit
                        2.0m,PlasticBar
                        4.0m,CopperCable]
    | Inserter -> [ 1.0m,Circuit
                    1.0m,Gear
                    1.0m,Iron]
    | FilterInserter -> [   1.0m,FastInserter
                            4.0m,Circuit]
    | FastInserter -> [ 2.0m,Circuit
                        2.0m,Iron
                        1.0m, Inserter]
    | Steel -> [ 5.0m,Iron]
    | x -> failwithf "Failed to reduce %A" x
    >> List.map (fun (i,x) -> x,i)
    >> Map.ofList
    
                        
type ItemAmountDisplay = decimal * string
type ReductionResult = {FullyReducedX:ItemMap;CanReduceX:ItemMap; Totals:ItemMap}
type ReductionResultDisplay = {
    FullyReduced:ItemAmountDisplay list
    CanReduce:ItemAmountDisplay list
    Total: ItemAmountDisplay list
    }
let getDumpResultDisplay x =
    let mapItems = Map.toList >> List.map(fun (item,i) -> i, sprintf "%A" item) 
    {   FullyReduced=mapItems x.FullyReducedX
        CanReduce= mapItems x.CanReduceX
        Total= mapItems x.Totals
        }
let dumpResult x = 
    getDumpResultDisplay x
    |> Dump
    |> ignore
let reduceOnce (y:Item,i:decimal) :ReductionResult = 
    let reduction = 
        reduceItem y
        |> Map.toList
        |> List.map(fun (y,x) -> y,x * i)
        |> Map.ofList
        
    let result =
        {   FullyReducedX = reduction |> Map.toList |> List.filter (fst >> canReduce >> not) |> Map.ofList
            CanReduceX=reduction |> Map.toList |> List.filter (fst >> canReduce)|> Map.ofList
            Totals= reduction
        }
    //dumpResult result
    result
let addMaps m1 m2 = 
    let summed = 
        m1 
        |> Map.map(fun k v ->
            if m2 |> Map.containsKey k then
                v + m2.[k]
            else v
        )
    summed
    |> Map.toList
    |> List.append (m2 |> Map.filter(fun k v -> summed.ContainsKey k |> not ) |> Map.toList)
    |> Map.ofList
    
    
let keepReducing x =
    let mutable x = reduceOnce x
    while (x.CanReduceX |> Seq.exists(fun _ -> true)) do
        (getDumpResultDisplay x).Dump("reducing")
        x <- 
            x.CanReduceX 
            |> Map.toList
            |> fun x -> x
            |> List.map (fun (y,i) -> reduceOnce (y,i))
            |> fun x -> x
            |> List.fold(fun result nextX -> 
                {result with 
                    FullyReducedX = nextX.FullyReducedX |> addMaps result.FullyReducedX
                    CanReduceX = nextX.CanReduceX |> addMaps result.CanReduceX
                    Totals = nextX.Totals |> addMaps result.Totals
                }) {x with CanReduceX = Map.empty}
        printfn "finished a loop"
        ()
[1.0m,Science3]
|> List.map (fun (i,x) -> x,i)
//|> List.map (fun (i,y) -> reduceItem y |> List.map(fun (x,y) -> x * i,y))
//|> List.map reduceOnce
|> List.map keepReducing
//|> List.concat
|> ignore

