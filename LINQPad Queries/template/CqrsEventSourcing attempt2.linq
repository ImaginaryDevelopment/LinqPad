<Query Kind="FSharpProgram" />

//ddd with cqrs/event sourcing?
// based off of http://blog.2mas.xyz/fsharp-event-sourcing-and-cqrs-tutorial-and-agents/
// if this attempt fails also try https://github.com/Thorium/SimpleCQRS-FSharp
// or http://gorodinski.com/blog/2013/02/17/domain-driven-design-with-fsharp-and-eventstore/
// or https://github.com/gregoryyoung/m-r/blob/master/SimpleCQRS/CommandHandlers.cs
[<AutoOpen>]
module ErrorHandling =

    type Result<'TResult, 'TError> =  
        | Success of 'TResult
        | Failure of 'TError
    
    let ok x = Success x  
    let fail x = Failure x  
    let bind f = function 
    | Success y -> f y
    | Failure err -> Failure err

    let (>>=) result func = bind func result
    
    let (>=>) f1 f2 = f1 >> (bind f2)
    
    
[<AutoOpen>]
module AgentHelper =
    type Agent<'T> = MailboxProcessor<'T>
    let post (agent:Agent<'T>) message = agent.Post message
    let postAsyncReply (agent:Agent<'T>) messageConstr = agent.PostAndAsyncReply(messageConstr)


[<AutoOpen>]
module EventStore =
    
    open System
    
    type StreamId = StreamId of int
    type StreamVersion = StreamVersion of int
    
    type SaveResult = 
        | Ok
        | VersionConflict
    
    type Messages<'T> = 
        | GetEvents of StreamId * AsyncReplyChannel<'T list option>
        | SaveEvents of StreamId * StreamVersion * 'T list * AsyncReplyChannel<SaveResult>
        | AddSubscriber of string * (StreamId * 'T list -> unit)
        | RemoveSubscriber of string
    
    type internal EventStoreState<'TEvent,'THandler> = 
        {
            EventHandler: 'THandler
            GetEvents: 'THandler -> StreamId -> ('TEvent list option * 'THandler) 
            SaveEvents: 'THandler -> StreamId -> StreamVersion -> 'TEvent list -> (SaveResult * 'THandler)
            Subscribers: Map<string, (StreamId * 'TEvent list -> unit)>
        }
    
    let eventStoreAgent<'T, 'TEventHandler> (eventHandler:'TEventHandler) getEvents saveEvents (inbox:Agent<Messages<'T>>) = 
        let initState = 
            {
                EventHandler = eventHandler
                Subscribers = Map.empty
                GetEvents = getEvents
                SaveEvents = saveEvents
            }
        let rec loop state = 
            async {
                let! msg = inbox.Receive()
                match msg with
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
    
    let createEventStoreAgent<'TEvent, 'TEventHandler> eventHandler getEvents saveEvents = 
        Agent.Start(eventStoreAgent<'TEvent, 'TEventHandler> eventHandler getEvents saveEvents)
    
    type EventStore<'TEvent, 'TError> = 
        {
            GetEvents: StreamId -> Result<StreamVersion*'TEvent list, 'TError>
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
    
        { GetEvents = getEvents; SaveEvents = saveEvents; AddSubscriber = addSubscriber; RemoveSubscriber = removeSubscriber}
    
    
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
    
        let agent = createEventStoreAgent initState getEventsInMap saveEventsInMap
        createEventStore<'TEvent, 'TError> versionError agent
        
        
module example = 
    let inMemoryEventStore = createInMemoryEventStore<string,string> "This is a version error"  
    inMemoryEventStore.AddSubscriber "FirstSubscriber" (printfn "%A")  
    let res0 = inMemoryEventStore.SaveEvents (StreamId 1) (StreamVersion 0) ["Hello";"World"]  
    let res1 = inMemoryEventStore.SaveEvents (StreamId 1) (StreamVersion 1) ["Hello2";"World2"]  
    let res2 = inMemoryEventStore.SaveEvents (StreamId 1) (StreamVersion 2) ["Hello2";"World2"]    
    
    [res0;res1;res2] |> List.iteri (fun i v -> printfn "%i: %A" i v)

module Contracts =
    [<AutoOpen>]
    module Types =
        
        type AggregateId = AggregateId of int
        type ItemId = ItemId of int
        
//        [<AutoOpen>]
//        module LibAAS.Contracts.Types
//        open System
//         
//        type AggregateId = AggregateId of int
//        
//        type ItemId = ItemId of int
//        type Title = Title of string
//        type Author = Author of string
//        type UserId = UserId of int
//        type LibraryId = LibraryId of int
//        type LoanId = LoanId of int
//        type Book = {Title: Title; Author: Author }
//        type Quantity = private Quantity of int
//            with 
//                static member Create x = 
//                    if x >= 0 then Quantity x
//                    else raise (exn "Invalid quantity")
//        type ItemData = 
//            | Book of Book
//        type Item = ItemId*ItemData
//        
//        type LoanDate = LoanDate of DateTime
//        type DueDate = DueDate of DateTime
//        
//        type ReturnDate = ReturnDate of DateTime
//        type Fine = Fine of int
//        
//        type Loan = { LoanId: LoanId; UserId: UserId; ItemId: ItemId; LibraryId: LibraryId }
//        
//        type Version = int
//        type Error = 
//            | NotImplemented of string
//            | VersionConflict of string
//            | InvalidStateTransition of string
//            | InvalidState of string
//            | InvalidItem


    [<AutoOpen>]
    module Commands = 
    
    
        type CommandData = 
            | Submit of ItemId*Claim // uses ItemId because the claim is saved before it is submitted // could this also encapsulate resubmits?
            | AddCharge of Charge 
            | DeleteCharge of ItemId
            | UpdateCharge of ItemId*Charge
        
        and Charge = {Amount:decimal; ProcedureCodeId:string}        
        
        and Claim = {Charges: Charge seq; }
        
//    [<AutoOpen>]
//    module LibAAS.Contracts.Commands
//    
//    type CommandData = 
//        | LoanItem of LoanItem
//        | ReturnItem of ReturnItem
//        | RegisterInventoryItem of RegisterInventoryItem
//    
//    and LoanItem = {
//        Id:LoanId
//        UserId:UserId
//        ItemId:ItemId
//        LibraryId:LibraryId }
//    
//    and ReturnItem = {
//        Id:LoanId }
//    
//    and RegisterInventoryItem = {
//        Item:Item
//        Quantity:Quantity }
//    
//    type Command = AggregateId * CommandData
//    


    [<AutoOpen>]        
    module Events = 
        open Commands
        type Error = 
            | NotImplemented of string
            | VersionConflict of string
            | InvalidStateTransition of string
            | InvalidState of string
            | InvalidItem
        
        type EventData = 
            |Resubmitted of Claim
            |Created of Claim
            |ChargeCreated of ItemId*Charge
            |Changed of Claim
            |AddChargeFailed
            |ClaimValidationFailed of Error seq
            |ChargeDeleted of Claim
            
        type Events = AggregateId * EventData list
        
//        [<AutoOpen>]
//        module LibAAS.Contracts.Events
//        open System
//        
//        type EventData = 
//            | ItemLoaned of loan:Loan*loanDate:LoanDate*dueDate:DueDate
//            | ItemRegistered of item:Item * Quantity:Quantity
//            | ItemReturned of loan:Loan*returnDate:ReturnDate
//            | ItemLate of loan:Loan*returnDate:ReturnDate*numberOfDaysLate:int*fine:Fine
//        
//        type Events = AggregateId * EventData list
        
        
open Contracts 
module Domain =
    module DomainTypes = // module LibAAS.Domain.DomainTypes
        type ChargeState =
        | ChargeInit // Init means initial state, no changes have been made to it summarized from https://github.com/mastoj/LibAAS/tree/master/ex1
        | Charge of Charge
    
        type ClaimData = {Claim: Claim; ClaimDate: DateTime (* ClaimDate*); DueDate: DateTime (* DueDate *) }
        type ClaimState = 
            | ClaimInit
            | ClaimCreated of ClaimData
            
        
        type EvolveOne<'T> = EventData -> 'T -> Result<'T, Error>
        type EvolveSeed<'T> = 
            {
                Init: 'T
                EvolveOne: EvolveOne<'T>
            }
        
        type InternalDependencies = 
            {
                GetItem: ItemId -> Result<ClaimState,Error>
            }
        
        type StateGetters = 
            {
                GetClaimItem: ItemId -> Result<ClaimState, Error>
            }
//        module internal LibAAS.Domain.DomainTypes
//
//        open LibAAS.Contracts
//        
//        type InventoryState =
//            | ItemInit
//            | ItemInStock of item:Item*quantity:Quantity
//        
//        type LoanData = {Loan: Loan; LoanDate: LoanDate; DueDate: DueDate}
//        type LoanState = 
//            | LoanInit
//            | LoanCreated of LoanData
//        
//        type EvolveOne<'T> = EventData -> 'T -> Result<'T, Error>
//        type EvolveSeed<'T> = 
//            {
//                Init: 'T
//                EvolveOne: EvolveOne<'T>
//            }
//        
//        type InternalDependencies = 
//            {
//                GetItem: ItemId -> Result<InventoryState,Error>
//            }
//        
//        type StateGetters = 
//            {
//                GetInventoryItem: ItemId -> Result<InventoryState, Error>
//            }
            
            
    module Charge = // adapted from Inventory.fs @ https://github.com/mastoj/LibAAS/blob/master/ex4/done/LibAAS.Domain/Inventory.fs
//        module internal LibAAS.Domain.Inventory
//
//        open LibAAS.Contracts
//        open LibAAS.Domain.DomainTypes
//        open System
//        
//        let handleAtInit (id, (command:RegisterInventoryItem)) = 
//            [ItemRegistered(command.Item, command.Quantity)] |> ok
//        
//        let executeCommand state command =
//            match state, command with
//            | ItemInit, (id, RegisterInventoryItem cmd) -> handleAtInit (id, cmd)
//            | _ -> InvalidState "Inventory" |> fail
//        
//        let evolveAtInit = function
//            | ItemRegistered(item, quantity) -> ItemInStock (item, quantity) |> ok
//        
//        let evolveOne (event:EventData) state = 
//            match state with
//            | ItemInit -> evolveAtInit event

//        let evolveSeed = {Init = ItemInit; EvolveOne = evolveOne}  

        let handleAtInit (aggId, (commandData:Charge)) = 
            // TODO: write to master command record?
            [ChargeCreated(commandData)] |> ok
        
    module Claim =
        ()
    
    module CommandHandling = ()
    module DomainEntry = ()
    
    //type ChargeCommand = | Delete | Add | Update
    
    
//    type ClaimCommand = 
//        |Resubmit of Claim
//        |DeleteCharge of Charge
//        |UpdateCharge of Charge
//        |Create of Claim
//        |AddCharge of NewCharge:Charge
 
    
    
    
    type Version = int

        
open Domain


 
// end module

let validateCommand command = command |> ok

let handleAtInit stateGetters ((aggId:AggregateId), (commandData:Claim)) = 
    commandData.ClaimId |> 
        (stateGetters.GetClaim
            >=> function
                | ItemInit -> InvalidItem |> fail
                | _ ->
                let loan = 
                    { LoanId = commandData.Id
                      UserId = commandData.UserId
                      ItemId = commandData.ItemId
                      LibraryId = commandData.LibraryId }
                let now = DateTime.Today
                [ItemLoaned (loan, LoanDate now, DueDate (now.AddDays(7.)))] |> ok)

let handleAtCreated data ((aggId:AggregateId), (commandData:ReturnItem)) =
    let now = DateTime.Today
    let (DueDate duedate) = data.DueDate
    let daysLate = (now - duedate).Days
    let fine = 100 * daysLate
    if now > duedate then 
        [ItemLate (data.Loan, ReturnDate now, daysLate, Fine fine )] |> ok
    else 
        [ItemReturned (data.Loan, ReturnDate now )] |> ok

let executeCommand state stateGetters command =
    match state, command with
    | LoanInit, (id, LoanItem data) -> handleAtInit stateGetters (id, data)
    | LoanCreated data, (id, ReturnItem cmd) -> (id, cmd) |> handleAtCreated data
    | _ -> InvalidState "Loan" |> fail

let evolveAtInit = function
    | ItemLoaned (loan, loanDate, dueDate) -> 
        LoanCreated {Loan = loan; DueDate = dueDate; LoanDate = loanDate} |> ok
    | _ -> InvalidStateTransition "Loan at init" |> fail

let evolveAtCreated data = function
    | _ -> raise (exn "Implement me")

let evolveOne (event:EventData) state = 
    match state with
    | LoanInit -> evolveAtInit event
    | _ -> raise (exn "Implement me")

let evolveSeed = {Init = LoanInit; EvolveOne = evolveOne}
let buildState2 evolveSeed (version, events) = 
    let evolver res e = bind (evolveSeed.EvolveOne e) res
    events |> List.fold evolver (evolveSeed.Init |> ok)

let stateBuilder evolveSeed getEvents id = 
    getEvents id >>= buildState2 evolveSeed

let buildState evolveSeed (aggregateId, version, events, command) =
    let evolver res e = bind (evolveSeed.EvolveOne e) res
    let state = events |> List.fold evolver (evolveSeed.Init |> ok)
    state >>= (fun s -> (aggregateId, version, s, command) |> ok)

let (|LoanCommand|InventoryCommand|) command =
    match command with
    | LoanItem _ -> LoanCommand
    | ReturnItem _ -> LoanCommand
    | RegisterInventoryItem _ -> InventoryCommand

let commandRouteBuilder stateGetters commandData =
    match commandData with
    | LoanCommand -> 
        (buildState Loan.evolveSeed)
        >=> (fun (_,_,s,command) -> Loan.executeCommand s stateGetters command)
    | InventoryCommand ->
        (buildState Inventory.evolveSeed)
        >=> (fun (_,_,s,command) -> Inventory.executeCommand s command)

let executeCommand stateGetters (aggregateId, currentVersion, events, command) =
    let (aggId, commandData) = command
    (aggregateId, currentVersion, events, command)
    |> commandRouteBuilder stateGetters commandData
    >>= (fun es -> (aggregateId, currentVersion, es, command) |> ok)

let getEvents2 eventStore (id) =
    eventStore.GetEvents (StreamId id)

let getEvents eventStore command = 
    let (AggregateId aggregateId, commandData) = command
    eventStore.GetEvents (StreamId aggregateId)
    >>= (fun (StreamVersion ver, events) -> (AggregateId aggregateId, ver, events, command) |> ok)

let saveEvents eventStore (AggregateId aggregateId, expectedVersion, events, command) = 
    eventStore.SaveEvents (StreamId aggregateId) (StreamVersion expectedVersion) events

let createGetters eventStore = 
    {
        GetInventoryItem = (fun (ItemId id) -> id |> stateBuilder Inventory.evolveSeed (getEvents2 eventStore))
    }
    
let execute eventStore command = 
    let stateGetters = createGetters eventStore

    command 
    |> validateCommand
    >>= getEvents eventStore
    >>= executeCommand stateGetters
    >>= saveEvents eventStore
    
let createApp() =
    let eventStore = createInMemoryEventStore<EventData, Error> (Error.VersionConflict "Version conflict")
    (eventStore, execute eventStore)
    
//let inMemoryEventStore = createInMemoryEventStore<ClaimCommand,Error> "This is a version error"
//inMemoryEventStore.AddSubscriber "FirstSubscriber" (printfn "%A")
