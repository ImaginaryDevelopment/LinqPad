<Query Kind="FSharpProgram" />

// tags
// #byte #formatting #cache #changeTracking

type Resource = {
    Rate: int
    Inventory: uint64 }

type ResourceEntry = Resource * DateTime
type SerializableRE = Resource * DateTime
type TrackType =
    | Goal of bool
    | OnRateChange of bool
    | Never // for moving windows like estimating how long it takes to save up for each ritual

// if F5 should record the values on run
let track = true

let thousand, million, billion = 1_000uL, 1_000_000uL, 1_000_000_000uL
let _abdicate = 171_200

let wheat = {Rate=4041; Inventory = 2880uL * thousand}
let wood = {Rate=2180; Inventory = 19uL * million}
let stone = {Rate=8925; Inventory = 25uL * million }
let faith = {Rate = 1852; Inventory = 513_000uL  } // 17.7 hours, 1064.3 minutes, Finished at 7/18/2023 4:05:36 PM
let hol = {Rate=18; Inventory = 0uL }

let targets =
    [
        "Cathedralx2 (stone)", (30uL * million), stone, Goal (track && false)
        "Religion (faith)", million, faith, OnRateChange (track && true)
        //"Level up(1500)", 25uL * million, wood
        "Hero", 5uL * million, wheat, OnRateChange (track && false)
        "Ritual(faith)", 140_600uL, {faith with Inventory = 0uL}, Never
        //"Tremors(hol)", 99_999uL, hol 
        //"Academy 2 (stone)", (100uL * billion), stone
    ]


let deserialize (text:string) =
    let r, dt = System.Text.Json.JsonSerializer.Deserialize<SerializableRE>(text)
    r, dt
    
let serialize (r,dt:DateTime) =
    let x = (r,dt)
    System.Text.Json.JsonSerializer.Serialize(x)
    
let summarizeWait (title: string) goal resource (asof: DateTime) =
    if resource.Inventory > goal then
        title, asof, None, "Ready"
    else
    let seconds = float (goal - resource.Inventory) / float resource.Rate
    let eta = asof.AddSeconds(seconds)
    
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
    |> String.concat " - "
    |> fun text ->
        title, asof, Some eta, text

// addNew is deciding, do we want to start tracking the new value we are testing with
let trackChanges key trackType (value,dt: DateTime) =
    let useCaching () =
        let mutable cacheValues : (Resource * DateTime) list = List.empty
        let fetchCache =
            fun forceUpdate ->
                let v = cacheValues |> List.map serialize |> Array.ofList
                // printfn "Key is %s" key
                Util.Cache<string[]>((fun () -> v), key= key,forceRefresh= forceUpdate)
                |> List.ofArray
                |> List.map deserialize
        let addNew () =  
            cacheValues <- (value,dt)::fetchCache false
            cacheValues // fetch false after this will use cache, rather than accessing local
        fetchCache, addNew
                
    match trackType with
    | OnRateChange false ->
        let fetchCache, _ = useCaching()
        match fetchCache false with
        | [] -> [value,dt]
        | (lastV,_)::_ as old ->
            if lastV <> value then
                (value,dt)::old
            else old
                
    | TrackType.Never -> [value,dt]
    
    | OnRateChange true ->
        let fetchCache,fAddNew = useCaching()
        match fetchCache false with
        | [] -> // no values, need to track at least 1 to determine rate change
            fAddNew() |> ignore
            fetchCache true
        | (lastV,_)::rem as old ->
            if lastV <> value then
                fAddNew() |> ignore
                fetchCache true
            else old
    | Goal trackNew ->
        let fetchCache,fAddNew = useCaching()        
        let next = fAddNew()
    
        if trackNew then // TODO: only addnew if the rate has changed, or maybe if the value deviates significantly from expected?
            // forces the save to the cache
            fetchCache true
        else
            next
    

targets
|> List.map(fun (title, req, resource, trackType) ->
    title, trackChanges title trackType (resource, DateTime.Now)
    |> List.map(fun (v,dt) ->
        let _, asof, eta, text = summarizeWait title req v dt
        asof, eta |> Option.map string |> Option.defaultValue "Ready", text
    )
)
|> Dump
|> ignore
printfn "%A" DateTime.Now