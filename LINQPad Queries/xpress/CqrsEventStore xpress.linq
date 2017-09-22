<Query Kind="FSharpProgram" />

// post eob CQRS + ES
// http://blog.2mas.xyz/fsharp-event-sourcing-and-cqrs-tutorial-and-agents/

let dumpt t x = x.Dump(description=t); x
let flip f x y = f y x
let interject f x = 
    f x
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
    
    type StreamId = StreamId of int with
        member x.Value = x |> function | StreamId x -> x
    
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
            GetEvents: StreamId -> StreamVersion*'TEvent list
            GetVersion: StreamId -> StreamVersion
            SaveEvents: StreamId -> StreamVersion -> 'TEvent list -> Result<'TEvent list, 'TError>
            AddSubscriber: string -> (StreamId * 'TEvent list -> unit) -> unit
            RemoveSubscriber: string -> unit
        }
    type ValidationArgs<'TEvent,'TError> = { StreamId: StreamId; GetPreviousEvents: Lazy<'TEvent list>; EventsToValidate: 'TEvent list}
    
    let createEventStore<'TEvent, 'TError> (versionError:'TError) fGetValidationErrors agent =
        let getEvents streamId : StreamVersion*'TEvent list = 
            let result = (fun r -> GetEvents (streamId, r)) |> postAsyncReply agent |> Async.RunSynchronously
            match result with
            | Some events -> StreamVersion (events |> List.length), events
            | None -> StreamVersion 0, []
    
        let saveEvents streamId expectedVersion events : Result<'TEvent list, 'TError> = 
            // fvalidate takes a function get get the old events, and the new events
            match fGetValidationErrors {GetPreviousEvents=lazy(getEvents streamId|> snd);EventsToValidate= events; StreamId=streamId} with
            // no errors
            | None -> 
                let result = (fun r -> SaveEvents(streamId, expectedVersion, events, r)) |> postAsyncReply agent |> Async.RunSynchronously
                match result with
                | Ok -> events |> ok
                | VersionConflict -> versionError |> fail
            | Some error ->
                error |> fail
                
    
        let addSubscriber subId subscriber = 
            (subId,subscriber) |> AddSubscriber |> post agent
    
        let removeSubscriber subId = 
            subId |> RemoveSubscriber |> post agent
        let getVersion streamId : StreamVersion = 
            let result = (fun r -> GetVersion(streamId,r)) |> postAsyncReply agent |> Async.RunSynchronously
            result

        { GetEvents = getEvents; SaveEvents = saveEvents; AddSubscriber = addSubscriber; RemoveSubscriber = removeSubscriber; GetVersion = getVersion}
    
    
    let createInMemoryEventStore<'TEvent, 'TError> (versionError:'TError) fGetValidationError=
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
        createEventStore<'TEvent, 'TError> versionError fGetValidationError agent
let bindSave (es:EventStore.EventStore<_,_>) sId items prevResult = 
    match prevResult with 
    | Success _ -> 
        let v = es.GetVersion sId
        es.SaveEvents sId v items
    | Failure x -> Failure x
open EventStore        
module Sample = 
    let run () = 
        let inMemoryEventStore = createInMemoryEventStore<string,string> "This is a version error" (fun _ -> None)
        
        inMemoryEventStore.AddSubscriber "FirstSubscriber" (printfn "%A")  
        let res0 = inMemoryEventStore.SaveEvents (StreamId 1) (StreamVersion 0) ["Hello";"World"]  
        res0.Dump("Sample.res0")
        let res1 = inMemoryEventStore.SaveEvents (StreamId 1) (StreamVersion 1) ["Hello2";"World2"]  
        let res2 = inMemoryEventStore.SaveEvents (StreamId 1) (StreamVersion 2) ["Hello2";"World2"]

        [res0;res1;res2] |> List.iteri (fun i v -> printfn "%i: %A" i v)
        
module ReadModels = 
    let printer (es:EventStore<_,_>) = es.AddSubscriber "Printer" (printfn "Printer:%A")
    
// aggregate roots area
module ClaimProcess =
    type Entity<'T> = {Value:'T; StreamId:StreamId; Version:StreamVersion}
    
    type [<Measure>] PatientId    
    
    type Patient = { LastName:String;FirstName:String;DoB:DateTime;Guarantor:int<PatientId> option }
    type PartialPatient = { LastName:String option;FirstName:String option;DoB:DateTime option;Guarantor:int<PatientId> option option } with
        static member Zero = {LastName=None;FirstName=None;DoB=None;Guarantor=None}
    type PatientE = Entity<Patient>
    let getPatientId (pe:PatientE) = pe.StreamId
        
    [<RequireQualifiedAccess>]
    type PatientCommand = 
        | Create of Patient 
        // how do we serialize this update?
        | Update of PartialPatient
//    
//    type ApptStatus = |Scheduled | CheckedIn | CheckedOut
//    type Appointment = { AppointmentId: Identity; StartDate: DateTime; Status:ApptStatus; Created:DateTime} with member x.StreamId = x.AppointmentId
//    type AppointmentEvent = 
//        | Create
    
    let cInMemory<'T> fValidate = 
        createInMemoryEventStore<'T, string> "This is a version error" fValidate
    
    let esPt = 
        let validation v = 
            match v.EventsToValidate |> Seq.exists(function | PatientCommand.Create _ -> true | _ -> false) with
            | true ->
                v.GetPreviousEvents.Value |> Seq.choose(function | PatientCommand.Create _ -> Some (sprintf "Patient already created for id: %i" (v.StreamId.Value)) | _ -> None)
                |> Seq.tryHead
            | false ->
                None
        cInMemory<PatientCommand> validation
    ReadModels.printer esPt
    type FoldProcess<'TState> = | Finished of 'TState |Proceed of 'TState
    let updateFromPartial (p:PartialPatient) (pt:Patient) = 
        let updateIf fProp fUpdate pt = 
            match fProp p with
            | Some x -> fUpdate x pt
            | None -> pt
        pt
        |> updateIf (fun pt -> pt.DoB) (fun dob pt -> {pt with DoB = dob})
        
        
    let fGetCurrent = 
        let current:Dictionary<StreamId, Patient> = Dictionary()
        esPt.AddSubscriber "Current" (fun (StreamId sId as s, events) -> 
            events
            |> Seq.fold(fun (fpPtOpt:FoldProcess<Patient option>) e ->
                match fpPtOpt,e with
                | Finished x, _ -> Finished x
                // create when there's already one? faulted, finish fold
                | Proceed (Some _ as ptOpt), PatientCommand.Create _ -> Finished ptOpt
                | Proceed None, PatientCommand.Create pt -> Some pt |> Proceed
                // update when there's no patient created? finish
                | Proceed None, PatientCommand.Update _ -> Finished None
                | Proceed (Some pt), PatientCommand.Update f -> pt |> updateFromPartial f |> Some |> Proceed
                
            ) (Proceed(if current.ContainsKey s then current.[s] |> Some else None))
            |> function
                | Proceed ptOpt -> ptOpt
                | Finished ptOpt -> ptOpt
            |> function
                | Some pt -> current.[s] <- pt
                | None -> current.Remove(s) |> ignore
            //if current.ContainsKey s then (s,current.[s]).Dump("current")
        )
        fun streamId -> if current.ContainsKey streamId then Some current.[streamId]  else None
    
    type FoldStatus<'T> = 
        | Proceed of StreamVersion
        | Failed of string*StreamVersion*('T)
    
    let applyCommands f (StreamId sId as s) v =
        Seq.fold(fun fs events ->
            match fs with
            | Proceed (StreamVersion sv as v) ->
                let result = f s v [events]
                result
                |> function
                    | Success (x: _ list) ->
                        Proceed (sv + x.Length |> StreamVersion)
                    | Failure msg ->
                        Failed (msg, v, events)
            | x -> x
        ) (Proceed v)
open ClaimProcess

// setup some sample event for my domain and run them
[<RequireQualifiedAccess>]
type CommandType = 
    |Patient of PatientCommand

let cmdStream = [
    CommandType.Patient
        (PatientCommand.Create {LastName="Reid";FirstName="Riley";DoB=DateTime(1991,7,8); Guarantor=None})
    CommandType.Patient
        (PatientCommand.Update ( { PartialPatient.Zero with DoB = Some <| DateTime(1991,7,9)}))
]

cmdStream
|> Seq.map (function | CommandType.Patient pc -> applyCommands esPt.SaveEvents (StreamId 1) (StreamVersion 0) [pc])
|> sprintf "%A"
|> Dump
|> ignore

fGetCurrent (StreamId 1)
|> sprintf "current:%A"
|> Dump
|> ignore