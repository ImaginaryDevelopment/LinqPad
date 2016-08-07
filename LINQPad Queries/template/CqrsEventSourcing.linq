<Query Kind="FSharpProgram" />

// adapted from https://github.com/gregoryyoung/m-r/blob/master/SimpleCQRS/CommandHandlers.cs
// actual code https://github.com/thinkbeforecoding/m-r/tree/FSharp/FsSimpleCQRS

module Commands =
    //[<Abstract>]
    
    type DeactivateInventoryItem =
        {
            InventoryItemId: Guid
            OriginalVersion: int // version I'm acting on, for checking to make sure my change only fires if the data the change was based on is still unchanged
        }
    
    type CreateInventoryItem =
        {
            InventoryItemId: Guid
            Name: string
        }
    
    type RenameInventoryItem =
        {
            InventoryItemId: Guid
            NewName: string
            OriginalVersion: int
        }
    
    type CheckInItemsToInventory =
        {
            InventoryItemId: Guid
            Count: int
            OriginalVersion: int
        }
    
    type RemoveItemsFromInventory =
        {
            InventoryItemId: Guid
            Count: int
            OriginalVersion: int
        }
        
    type Command =
        |DeactivateInventoryItem of DeactivateInventoryItem
        |CreateInventoryItem of CreateInventoryItem
        |RenameInventoryItem of RenameInventoryItem
        |CheckInItemsToInventory of CheckInItemsToInventory
        |RemoveItemsFromInventory of RemoveItemsFromInventory
        
    type ICommandSender =
        abstract member Send : Command -> unit 
        
module Events =    
    
    type EventMetadata = {Version : int}
    
    type InventoryItemDeactivated =
        { Id: Guid }
    
    type InventoryItemCreated =
        {   Id: Guid
            Name: string }
    
    type InventoryItemRenamed =
        {   Id: Guid
            NewName: string }
    
    type ItemsCheckedInToInventory =
        {   Id: Guid
            Count: int }
    
    type ItemsRemovedFromInventory =
        {   Id: Guid
            Count: int }
    
    type Event = 
        |InventoryItemDeactivated of InventoryItemDeactivated
        |InventoryItemCreated of InventoryItemCreated
        |InventoryItemRenamed of InventoryItemRenamed
        |ItemsCheckedInToInventory of ItemsCheckedInToInventory
        |ItemsRemovedFromInventory of ItemsRemovedFromInventory
        
    type VersionedEvent = {Version:int; Event:Event}
        
module EventStores =
    open Events
    type IEventStore =
        abstract member GetEventsForAggregate : Guid -> Event seq
        abstract member SaveEvents : Guid -> int -> Event seq -> unit
        
        
module Domain =
    open Events
    module InventoryItem =
       
        type State = 
            {
                Id: Guid
                Activated: bool
            }
    
        let fire (o:Event) = [o]
    
        let rename newName s =
            if String.IsNullOrEmpty(newName) then raise (ArgumentException "newName")
            InventoryItemRenamed {InventoryItemRenamed.Id= s.Id; NewName = newName}
            |> fire 
    
        let remove count s =
            if count <= 0 then raise (InvalidOperationException "cant remove negative count from inventory")
            ItemsRemovedFromInventory {ItemsRemovedFromInventory.Id = s.Id; Count = count} 
            |> fire
    
        let checkIn count s =
            if count <= 0 then raise (InvalidOperationException "must have a count greater than 0 to add to inventory")
            ItemsCheckedInToInventory {ItemsCheckedInToInventory.Id= s.Id; Count = count} 
            |> fire
        
        let deactivate s =
            if not s.Activated then raise (InvalidOperationException "already deactivated")
            InventoryItemDeactivated {InventoryItemDeactivated.Id = s.Id}
            |> fire
    
        let create id name =
            InventoryItemCreated {InventoryItemCreated.Id = id; Name = name}
            |> fire
    
        let applyOnInventoryItem s (e: Event) =
            match e with
            | InventoryItemCreated e -> {Id = e.Id; Activated = true } 
            | InventoryItemDeactivated _e -> {s with Activated = false; }
            | _ -> s
    
        let replay = Seq.fold
    
        let replayInventoryItem  events =
            let empty = { Id = Guid.Empty; Activated = false} 
            replay applyOnInventoryItem empty events
            
            
module CommandHandlers =
    open Commands
    open Events
    open EventStores
    open Domain
    open InventoryItem
    
    type InventoryCommandHandlers (eventStore: IEventStore) =
        let load id = eventStore.GetEventsForAggregate id |> replayInventoryItem
        let save = eventStore.SaveEvents
    
        // load aggregate, execute f on it, then save
        let applyOn id version f =
            load id 
            |> f 
            |> save id version
    
        member x.Handle (c: CreateInventoryItem) =
            create c.InventoryItemId c.Name |> 
            save c.InventoryItemId -1
        
        member x.Handle (c: DeactivateInventoryItem) = 
            deactivate |> 
            applyOn c.InventoryItemId c.OriginalVersion
        
        member x.Handle (c: RemoveItemsFromInventory) =
            remove c.Count |> 
            applyOn c.InventoryItemId c.OriginalVersion
        
        member x.Handle (c: CheckInItemsToInventory) =
            checkIn c.Count |> 
            applyOn c.InventoryItemId c.OriginalVersion
        
        member x.Handle (c: RenameInventoryItem) =
            rename c.NewName |> 
            applyOn c.InventoryItemId c.OriginalVersion


module ReadModels =
    open Events
    type InventoryItemListDto = 
        {
            Id: Guid
            mutable Name: string 
        }
    
    type InventoryItemDetailsDto =
        {
            Id: Guid
            mutable Name: string
            mutable CurrentCount: int
            mutable Version: int
        }
    
    type IDatabase =
        abstract member Details : Dictionary<Guid, InventoryItemDetailsDto>
        abstract member List : List<InventoryItemListDto>
    
    type InventoryListView(database: IDatabase) =
        member x.Handle (e: InventoryItemCreated) =
            database.List.Add({ Id = e.Id; Name = e.Name })
        
        member x.Handle (e: InventoryItemRenamed) =
            let item = database.List.Find(fun x -> x.Id = e.Id)
            item.Name <- e.NewName
        
        member x.Handle (e: InventoryItemDeactivated) =
            database.List.RemoveAll (fun x -> x.Id = e.Id) |> ignore
    
    
    type InventoryItemDetailView(database: IDatabase) =
        let find id = database.Details.[id]
        
        member x.Handle (e: InventoryItemCreated, m : EventMetadata ) =
            database.Details.Add(e.Id, {Id = e.Id; Name = e.Name; CurrentCount = 0; Version= m.Version})
        
        member x.Handle (e: InventoryItemRenamed, m : EventMetadata ) =
            let item = find e.Id
            item.Name <- e.NewName
            item.Version <- m.Version
        
        member x.Handle (e: ItemsRemovedFromInventory, m : EventMetadata ) =
            let item = find e.Id
            item.CurrentCount <- item.CurrentCount - e.Count
            item.Version <- m.Version
    
        member x.Handle (e: ItemsCheckedInToInventory, m : EventMetadata ) =
            let item = find e.Id
            item.CurrentCount <- item.CurrentCount + e.Count
            item.Version <- m.Version
        
        member x.Handle (e: InventoryItemDeactivated) =
            database.Details.Remove(e.Id) |> ignore

module MyEventStore =
    open Events
    open EventStores
        
    type private EventDescriptor = { EventData:Event; Id:Guid; Version:int}
    
    type EventStore() = // based off of https://github.com/gregoryyoung/m-r/blob/master/SimpleCQRS/EventStore.cs since no F# version was found
        
        let current = Dictionary<Guid,List<EventDescriptor>>() //map of guid -> events (reversed)
        
        member x.GetEventsForAggregate guid = current.[guid] |> Seq.map (fun ed -> ed.EventData)
        member x.SaveEvents guid expectedVersion (newEvents:Event seq) = 
            "saving events".Dump()
            if not <| current.ContainsKey guid then
                "adding new aggregate".Dump()
                current.[guid] <- List<_>() 
            elif current.[guid].[current.[guid].Count - 1].Version <> expectedVersion && expectedVersion <> -1 then
                raise <| new System.Data.DBConcurrencyException()
            
            let mutable i = expectedVersion
            current.Dump("yay adding events")
            let eventDescriptors = current.[guid]
            newEvents
            |> Seq.iter (fun e ->
                i <- i + 1
                eventDescriptors.Add {EventData=e; Id=guid; Version = i}
                
                //TODO: |> publisher.Publish
            )
        
        interface IEventStore with
            member x.GetEventsForAggregate guid = guid |> x.GetEventsForAggregate
            member x.SaveEvents guid expectedVersion newEvents = x.SaveEvents guid expectedVersion newEvents

open MyEventStore

let store = EventStore()

let wreckItAggregate = Guid.NewGuid()
[Events.Event.InventoryItemCreated { Events.InventoryItemCreated.Id=Guid.NewGuid(); Name="I'm Gonna Wreck It underpants"}]
|> store.SaveEvents (wreckItAggregate) -1 
store.Dump()
store.GetEventsForAggregate wreckItAggregate
|> Dump
