<Query Kind="FSharpProgram" />

// tags
// #byte #formatting #cache #changeTracking

type Resource = {
    Rate: int
    Inventory: uint64 }

type ResourceEntry = Resource * DateTime
type SerializableRE = int * uint64 * int64
    
// if F5 should record the values on run
let track = false

let million, billion = 1_000_000uL, 1_000_000_000uL
let _abdicate = 171_200

let wheat = {Rate=825; Inventory = 41_000uL }
let wood = {Rate=50; Inventory = 14_000uL }
let stone = {Rate=770; Inventory = 137uL * million }
let faith = {Rate = 487; Inventory = 0uL  } // 17.7 hours, 1064.3 minutes, Finished at 7/18/2023 4:05:36 PM

let hol = {Rate=13; Inventory = 95_600uL}

let target =
    [
        //"Ritual(faith)", 325_200uL, faith
        "StoneSkin(hol)", 99_999uL, hol 
        "Academy 2 (stone)", (100uL * billion), stone
    ]
    |> List.head

let deserialize (rate,inventory, dtTicks) =
    {Rate=rate;Inventory=inventory}, DateTime(ticks = dtTicks)
    
let serialize (r,dt:DateTime) =
    r.Rate, r.Inventory, dt.Ticks
    
let summarizeWait title goal resource (asof: DateTime) =
    if resource.Inventory > goal then printfn "%s is ready" title
    else
    let seconds = float (goal - resource.Inventory) / float resource.Rate
    let eta = asof.AddSeconds(seconds)
    let titling = sprintf "%s - %A ~ %A" title asof eta
    
    [
        yield $"%.1f{seconds} seconds"
        if seconds > 120. then
            let minutes = seconds / 60.
            yield $"%.1f{minutes} minutes"
            if minutes > 60. then
                let hours = minutes / 60.
                yield $"%.1f{hours} hours"
                if hours > 48. then
                    let days = hours / 24.
                    yield $"%.1f{days} days"
    ]
    |> List.rev
    |> List.truncate 2
    //|> fun x -> $"Finished at %A{eta}"::x
    //|> List.rev
    //|> fun x -> $"Started at %A{now}"::x
    //|> List.rev
    |> fun x -> x.Dump(titling)

// addNew is deciding, do we want to start tracking the new value we are testing with
let trackChanges title addNew (value,dt: DateTime) = 
    let mutable cacheValues : ResourceEntry list = List.empty
    let fetchCache =
        fun forceUpdate ->
            let v = cacheValues |> List.map serialize |> Array.ofList
            Util.Cache<SerializableRE[]>((fun () -> v), key= title,forceRefresh= forceUpdate)
            |> List.ofArray
            |> List.map deserialize
            
    cacheValues <- (value,dt)::fetchCache false
    if addNew then
        fetchCache true
    else cacheValues
    

let title, goal, resource = target
trackChanges "Faith" track (resource, DateTime.Now)
|> List.iter(fun (v,dt) ->
    summarizeWait title goal v dt
)
printfn "%A" DateTime.Now