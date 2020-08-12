<Query Kind="FSharpProgram" />

// conway's game of wife
let dumpt title x = 
    x.Dump(description=title)
let dumptf title x =
    Util.WithStyle(x,"font-family: Consolas").Dump(description=title)
type State =
    |Alive
    |Dead
    
type Spot = {State:State;X:int;Y:int}

let getDirections x y =
        [
            for dx in [-1..1] do
            for dy in [-1..1] do
                let x' = x + dx
                let y' = y + dy
                if x' <> x || y' <> y then
                    yield x',y'
        ]
let isAlive =
    function
    |Alive -> true
    | _ -> false
let containsPoint (items: _ list) (x,y) =
    x >= 0 && x < List.length items
    && y>=0 && y < List.length items.[x]
    
module Conway =
    let getLiveNeighborCount (items: _ list) x y =
        // how many columns
        getDirections x y
        |> List.filter (containsPoint items)
        |> List.filter(fun (x,y) -> items.[x].[y] |> isAlive)
        |> List.length
    
    let processOne neighborCount state =
        match state,neighborCount with
        | Alive, x when x < 2 -> Dead
        | Alive, x when x = 2 || x = 3 -> Alive
        | Alive, _ -> Dead
        | Dead, 3 -> Alive
        | _ -> Dead
        
    let processAll items =
        items
        |> List.mapi(fun x columns -> 
            columns |> List.mapi(fun y state ->
                let liveCount = getLiveNeighborCount items x y
                processOne liveCount state
            )
        )
let sampleInput1 =
    let d = Dead
    let allDead = List.replicate 10 d
    let a = Alive
    [
        allDead
        [ d;d;d;a;a;d;d;d;d;d]
        [ d;d;d;d;a;d;d;d;d;d]
        allDead
        allDead
    ]
let toDisplay (_,_,_,x) = match x with |Alive -> "*" | Dead -> "."

let show title f items =
    [
        for x in [0 .. List.length items - 1] do
        yield [
            for y in [0.. List.length items.[x] - 1] do
                yield f (items,x,y,items.[x].[y])
            ]
    ]
    |> List.map (String.concat null)
    |> dumptf title
    
sampleInput1
|> Conway.processAll 
|> show "sample output1" toDisplay

sampleInput1
|> show "sample input1" toDisplay
sampleInput1
|> show "liveCount" (fun (items,x,y,_) -> Conway.getLiveNeighborCount items x y |> string)


Conway.getLiveNeighborCount sampleInput1 0 2
|> Dump
|> ignore