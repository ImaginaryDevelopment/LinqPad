<Query Kind="FSharpProgram" />

// https://codingdojo.org/kata/GameOfLife/
// assume finite grid

type Cell = {x:int;y:int;Life:bool}
    with
        member x.ToDump() = if x.Life then "*" else "."
        override x.ToString() = x.ToDump()
    
// x,y

type NeighborView = {
    Neighbors: Cell list
}

let getNeighbors (grid: Cell[][]) (x,y) =
    let xLen = Array.length grid 
    if xLen < 1 then
        None
    else
        let yLen = Array.length grid[0]
        let neighbors = [
            // check 3 north
            if y > 0 then
                let localY = y - 1
                // northwest
                if x > 0 then
                    grid[x - 1][localY]
                // north
                grid[x][localY]
                // north east
                if x + 1 < xLen then
                    grid[x + 1][localY]
            // west
            if x > 0 then
                grid[x - 1][y]
            // east
            if x < xLen - 1 then
                grid[x+1][y]
            // south values
            if y + 1 < yLen then
                let localY = y + 1
                // south west
                if x > 0 then
                    grid[x - 1][localY]
                // south
                grid[x][localY]
                // south east
                if x < xLen - 1 then
                    grid[x + 1][localY]
                
        ]
        Some {Neighbors = neighbors}
        
let visitGrid f (model: Cell[][]) =
    (List.empty, model)
    ||> Array.fold(fun nextRow column ->
        let r =
            (List.empty, column)
            ||> Array.fold(fun nextColumn cell ->
                let n = getNeighbors model (cell.x, cell.y)
                (f n cell)::nextColumn
            )
        r::nextRow
    )
    |> List.map List.rev
    |> List.rev

let update (model: Cell[][]) =
    visitGrid (fun neighbors cell ->
            let live = neighbors |> Option.map(fun n -> n.Neighbors) |> Option.defaultValue List.empty |> List.filter(fun c -> c.Life) |> List.length
            if cell.Life then
                match live with
                | 2 | 3 -> cell
                | i -> if i < 2 || 3 < i then {cell with Life = false} else cell
            else
                if live = 3 then {cell with Life = true} else cell
    ) model
    |> List.map Array.ofList
    |> Array.ofList
let initialLife = [
   5,1
   4,2
   5,2
]

let grid = Array.init 8 (fun x -> Array.init 4 (fun y -> {x=x;y=y;Life= initialLife |> List.exists(fun (x2,y2) -> x = x2 && y = y2) }))

let addBorder f (grid: _[][]) =
    let lenX = Array.length grid
    if lenX < 1 then failwith "Empty grid"
    let lenY = Array.length grid[0]
    if lenY < 1 then failwith "Empty column"
    let bordered = Array.init(lenX+1) (fun x -> Array.init (lenY+1) (fun y -> if x = 0 && y = 0 then "x,y" elif x = 0 then $"y{y - 1}" elif y = 0 then $"x{x-1}" else f (grid[x - 1][y - 1])))
    bordered

let dumpGrid (grid:Cell[][]) =
    let display = addBorder string grid
    Util.HorizontalRun(false, display)
    |> Dump
    
//grid
//|> Dump
//|> ignore

(
    let x, y = 1,1
    let c = grid[x][y]
    getNeighbors grid (x,y)
)
|> Dump
|> ignore

grid
|> update
|> dumpGrid
|> ignore