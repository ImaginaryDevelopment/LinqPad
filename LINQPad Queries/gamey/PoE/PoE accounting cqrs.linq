<Query Kind="FSharpProgram" />

// path of exile accounting
// http://blog.2mas.xyz/fsharp-event-sourcing-and-cqrs-tutorial-and-agents/

let dumpt t x = x.Dump(description=t); x
let flip f x y = f y x
let interject f x = 
    f() 
    |> ignore
    x
module Option = 
    let getOrDefault x = function | Some x -> x | None -> x
module ErrorHandling = 
    type Result<'TResult, 'TError> =  
        | Success of 'TResult
        | Failure of 'TError
    
    let ok x = Success x  
    let fail x = Failure x
open ErrorHandling

module EventStore =
    module AgentHelper = 
        type Agent<'T> = MailboxProcessor<'T>  
        let post (agent:Agent<'T>) message = agent.Post message  
        let postAsyncReply (agent:Agent<'T>) messageConstr = agent.PostAndAsyncReply(messageConstr)  
    open AgentHelper
    
    type StreamId = StreamId of int
    
    type StreamVersion = StreamVersion of int
    type SaveResult = | Ok | VersionConflict
    
    type Messages<'T> = 
        | GetVersion of StreamId * AsyncReplyChannel<StreamVersion>
        | GetEvents of StreamId * AsyncReplyChannel<'T list option>
        | SaveEvents of StreamId * StreamVersion * 'T list * AsyncReplyChannel<SaveResult>
        | AddSubscriber of string * (StreamId * 'T list -> unit)
        | RemoveSubscriber of string
   
    type internal EventStoreState<'TEvent,'THandler> =  
        {
            EventHandler: 'THandler
            GetVersion: 'THandler -> StreamId -> (StreamVersion *'THandler)
            GetEvents: 'THandler -> StreamId -> ('TEvent list option * 'THandler) 
            SaveEvents: 'THandler -> StreamId -> StreamVersion -> 'TEvent list -> (SaveResult * 'THandler)
            Subscribers: Map<string, (StreamId * 'TEvent list -> unit)>
        }

    let eventStoreAgent<'T, 'TEventHandler> (eventHandler:'TEventHandler) getVersion getEvents saveEvents (inbox:Agent<Messages<'T>>) = 
        let initState = 
            {
                EventHandler = eventHandler
                Subscribers = Map.empty
                GetEvents = getEvents
                GetVersion = getVersion
                SaveEvents = saveEvents
            }
        let rec loop state = 
            async {
                let! msg = inbox.Receive()
                match msg with
                | GetVersion (id,replyChannel) ->
                    let (version,newHandler) = state.GetVersion state.EventHandler id
                    replyChannel.Reply(version)
                    return! loop {state with EventHandler = newHandler}
                | GetEvents (id, replyChannel) ->
                    let (events, newHandler) = state.GetEvents state.EventHandler id
                    replyChannel.Reply(events)
                    return! loop {state with EventHandler = newHandler}
                | SaveEvents (id, expectedVersion, events, replyChannel) ->
                    let (result, newHandler) = state.SaveEvents state.EventHandler id expectedVersion events
                    if result = Ok then state.Subscribers |> Map.iter (fun _ sub -> sub(id, events)) else ()
                    replyChannel.Reply(result)
                    return! loop {state with EventHandler = newHandler}
                | AddSubscriber (subId, subFunction) ->
                    let newState = {state with Subscribers = (state.Subscribers |> Map.add subId subFunction)}
                    return! loop newState
                | RemoveSubscriber subId ->
                    let newState = {state with Subscribers = (state.Subscribers |> Map.remove subId )}
                    return! loop newState
            }
        loop initState
    
    let createEventStoreAgent<'TEvent, 'TEventHandler> eventHandler getVersion getEvents saveEvents = 
        Agent.Start(eventStoreAgent<'TEvent, 'TEventHandler> eventHandler getVersion getEvents saveEvents)
    
    type EventStore<'TEvent, 'TError> = 
        {
            GetEvents: StreamId -> Result<StreamVersion*'TEvent list, 'TError>
            GetVersion: StreamId -> StreamVersion
            SaveEvents: StreamId -> StreamVersion -> 'TEvent list -> Result<'TEvent list, 'TError>
            AddSubscriber: string -> (StreamId * 'TEvent list -> unit) -> unit
            RemoveSubscriber: string -> unit
        }
    
    let createEventStore<'TEvent, 'TError> (versionError:'TError) agent =
        let getEvents streamId : Result<StreamVersion*'TEvent list, 'TError> = 
            let result = (fun r -> GetEvents (streamId, r)) |> postAsyncReply agent |> Async.RunSynchronously
            match result with
            | Some events -> (StreamVersion (events |> List.length), events) |> ok
            | None -> (StreamVersion 0, []) |> ok
    
        let saveEvents streamId expectedVersion events : Result<'TEvent list, 'TError> = 
            let result = (fun r -> SaveEvents(streamId, expectedVersion, events, r)) |> postAsyncReply agent |> Async.RunSynchronously
            match result with
            | Ok -> events |> ok
            | VersionConflict -> versionError |> fail
    
        let addSubscriber subId subscriber = 
            (subId,subscriber) |> AddSubscriber |> post agent
    
        let removeSubscriber subId = 
            subId |> RemoveSubscriber |> post agent
        let getVersion streamId : StreamVersion = 
            let result = (fun r -> GetVersion(streamId,r)) |> postAsyncReply agent |> Async.RunSynchronously
            result

        { GetEvents = getEvents; SaveEvents = saveEvents; AddSubscriber = addSubscriber; RemoveSubscriber = removeSubscriber; GetVersion = getVersion}
    
    
    let createInMemoryEventStore<'TEvent, 'TError> (versionError:'TError) =
        let initState : Map<StreamId, 'TEvent list> = Map.empty
    
        let saveEventsInMap map id expectedVersion events = 
            match map |> Map.tryFind id with
            | None -> 
                (Ok, map |> Map.add id events)
            | Some existingEvents ->
                let currentVersion = existingEvents |> List.length |> StreamVersion
                match currentVersion = expectedVersion with
                | true -> 
                    (Ok, map |> Map.add id (existingEvents@events))
                | false -> 
                    (VersionConflict, map)
    
        let getEventsInMap map id = Map.tryFind id map, map
        let getVersion map id = Map.tryFind id map |> Option.map List.length |> Option.getOrDefault 0 |> StreamVersion, map
        let agent = createEventStoreAgent initState getVersion getEventsInMap saveEventsInMap
        createEventStore<'TEvent, 'TError> versionError agent
let bindSave (es:EventStore.EventStore<_,_>) sId items prevResult = 
    match prevResult with 
    | Success _ -> 
        let v = es.GetVersion sId
        es.SaveEvents sId v items
    | Failure x -> Failure x
open EventStore        
module Sample = 
    let run () = 
        let inMemoryEventStore = createInMemoryEventStore<string,string> "This is a version error"  
        inMemoryEventStore.AddSubscriber "FirstSubscriber" (printfn "%A")  
        let res0 = inMemoryEventStore.SaveEvents (StreamId 1) (StreamVersion 0) ["Hello";"World"]  
        res0.Dump("Sample.res0")
        let res1 = inMemoryEventStore.SaveEvents (StreamId 1) (StreamVersion 1) ["Hello2";"World2"]  
        let res2 = inMemoryEventStore.SaveEvents (StreamId 1) (StreamVersion 2) ["Hello2";"World2"]

        [res0;res1;res2] |> List.iteri (fun i v -> printfn "%i: %A" i v)

module Accounting = 
    // later we can use UoM or tuple or whatever once we decide
    type Currency = decimal
    type Entry = { Name:string; Price:Currency; Amount:int}
    
            
    let sId = StreamId 1
    let es = createInMemoryEventStore<Entry,string> "This is a version error"
    
    let balanceRoot = 
        let dc = DumpContainer() |> dumpt "Balances"
        let balances = Dictionary<StreamId,Currency>()
        dc.Content <- balances
        es.AddSubscriber "balanceRoot" 
            (fun (sId,items) -> 
                printfn "subscriber activated"
                let prevBalance = if balances.ContainsKey sId then balances.[sId] else 0m
                printfn "retrieved old balance"
                let b =  items |> Seq.sumBy(fun x -> x.Price * decimal x.Amount) |> flip (+) prevBalance
                printfn "calculated new balance for %A is %A" sId b
                if prevBalance <> b then
                    balances.[sId] <- b
                    dc.Refresh()
                ()
            )
    
    [
        [{Name="Carnage Heart"; Amount = 1; Price=12m}]
        [{Name="Gale Coil Two-Stone Ring";Amount=1;Price=6m}]
        [{Name="Empyrean Spiral Two-Stone Ring"; Amount=1; Price=3m}]
        [{Name="Vortex Goad Hydrascale Boots"; Amount=1; Price=6m}]
        [{Name="Dyadus"; Amount=1; Price=4m}]
    ]
    |> Seq.fold(fun (vOpt) items ->
        match vOpt with
        | Some v ->
            let r = es.SaveEvents sId (StreamVersion v) items
            let l = items |> List.length
            let nextV = v + l
            Some nextV
        | None -> None
    ) (Some 0)
    |> ignore
    es.GetEvents sId
    |> dumpt "end"
    |> ignore

    
()