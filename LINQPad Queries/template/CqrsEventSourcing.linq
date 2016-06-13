<Query Kind="FSharpProgram" />

// adapted from https://github.com/gregoryyoung/m-r/blob/master/SimpleCQRS/CommandHandlers.cs
// actual code https://github.com/thinkbeforecoding/m-r/tree/FSharp/FsSimpleCQRS

module Commands =

    type Command =
        interface
        end
    
    type ICommandSender =
        abstract member Send : Command -> unit
    
    type DeactivateInventoryItem =
        {
            InventoryItemId: Guid
            OriginalVersion: int
        }
        interface Command
    
    type CreateInventoryItem =
        {
            InventoryItemId: Guid
            Name: string
        }
        interface Command
    
    type RenameInventoryItem =
        {
            InventoryItemId: Guid
            NewName: string
            OriginalVersion: int
        }
        interface Command
    
    type CheckInItemsToInventory =
        {
            InventoryItemId: Guid
            Count: int
            OriginalVersion: int
        }
        interface Command
    
    type RemoveItemsFromInventory =
        {
            InventoryItemId: Guid
            Count: int
            OriginalVersion: int
        }
        interface Command
        
        
module Events =    
    type IEvent = 
        interface
        end
    
    type EventMetadata ={Version : int}
    
    type InventoryItemDeactivated =
        { Id: Guid }
        interface IEvent
    
    type InventoryItemCreated =
        {   Id: Guid
            Name: string }
        interface IEvent
    
    type InventoryItemRenamed =
        {   Id: Guid
            NewName: string }
        interface IEvent
    
    type ItemsCheckedInToInventory =
        {   Id: Guid
            Count: int }
        interface IEvent
    
    type ItemsRemovedFromInventory =
        {   Id: Guid
            Count: int }
        interface IEvent
        
        
module EventStores =
    open Events
    type IEventStore =
        abstract member GetEventsForAggregate : Guid -> IEvent seq
        abstract member SaveEvents : Guid -> int -> IEvent seq -> unit
        
        
module Domain =
    open Events
    module InventoryItem =
       
        type State = 
            {
                Id: Guid
                Activated: bool
            }
    
        let fire o =
            [o :> IEvent]
    
        let rename newName s =
            if String.IsNullOrEmpty(newName) then raise (ArgumentException "newName")
            fire {InventoryItemRenamed.Id= s.Id; NewName = newName}
    
        let remove count s =
            if count <= 0 then raise (InvalidOperationException "cant remove negative count from inventory")
            fire {ItemsRemovedFromInventory.Id = s.Id; Count = count} 
    
        let checkIn count s =
            if count <= 0 then raise (InvalidOperationException "must have a count greater than 0 to add to inventory")
            fire {ItemsCheckedInToInventory.Id= s.Id; Count = count} 
        
        let deactivate s =
            if not s.Activated then raise (InvalidOperationException "already deactivated")
            fire {InventoryItemDeactivated.Id = s.Id}
    
        let create id name =
            fire {InventoryItemCreated.Id = id; Name = name}
    
        let applyOnInventoryItem s (e: IEvent) =
            match e with
            | :? InventoryItemCreated as e -> {Id = e.Id; Activated = true } 
            | :? InventoryItemDeactivated as e -> {s with Activated = false; }
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

//let store = EventStore()