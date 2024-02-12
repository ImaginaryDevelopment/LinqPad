<Query Kind="FSharpProgram" />

// follow Greg Young's simple CQRS pattern - https://www.youtube.com/watch?v=S2KLFlM_Z4I
[<AbstractClass>]
type Event() = class end
type Event<'t>() =
    inherit Event()
    
type Identifier = Guid
type IRepository<'t,'tEvent when 't :> AggregateRoot<'t,'tEvent>> =
    abstract member SaveEvents: Identifier -> Event seq -> int -> unit
    abstract member GetById : Identifier -> 't option
and Storage<'t,'tEvent when 't :> AggregateRoot<'t,'tEvent>> =
    abstract member GetEventsForAggregate:Identifier -> Event seq
    abstract member GetById:Identifier -> 't option
and AggregateRoot<'t,'tEvent> =
    abstract member Id:Identifier with get
    abstract member LoadFromHistory: 'tEvent seq -> 't
    
// private to prevent anything besides our own apply from creating new instances, is good idea?
type InventoryItem = private {id:Identifier;name:string;version:int;activated:bool} with
    member x.Id = x.id
    member x.Name= x.name
    member x.Version = x.version
    member x.Activated = x.activated
    member private x.ToDump() = sprintf "%A" x
    static member Zero = {id=Guid.Empty;name=null;version=0;activated=false}
    static member Apply x =
        function
        |InventoryItemCreated ident -> // how do we handle the identity? or prevent reactivation?
            if x.activated then 
                invalidOp "already activated"
            else 
                {x with activated=true;id=ident}
        |InventoryItemCheckedIn _ -> x
        |InventoryItemCheckedOut _ -> x
        |InventoryItemDeactivated _ -> {InventoryItem.Zero with id = x.id}
        |InventoryItemRenamed newName -> {x with name=newName}
    interface AggregateRoot<InventoryItem,InventoryItemEvent> with
        member x.Id with get() = x.Id
        member x.LoadFromHistory (events:InventoryItemEvent seq) =
            (x,events) ||> Seq.fold InventoryItem.Apply
and InventoryItemEvent =
    |InventoryItemDeactivated
    |InventoryItemCheckedIn
    |InventoryItemCheckedOut
    |InventoryItemRenamed of newName:string
    |InventoryItemCreated of Identifier
    with member private x.ToDump() = sprintf "%A" x

    
type Repository<'t,'tEvent when 't :> AggregateRoot<'t,'tEvent>>(storage:Storage<'t,'tEvent>) =
    interface IRepository<'t,'tEvent> with
        member __.SaveEvents _ident _events _expectedVersion = ()
        member __.GetById ident = storage.GetById ident
        
            
let events = [
    InventoryItemCreated (Guid.NewGuid())
    InventoryItemCheckedIn
    InventoryItemCheckedIn
    InventoryItemRenamed "Hello CQRS"
    InventoryItemCheckedOut
    InventoryItemDeactivated
]
let foldHooker f =
    fun x y ->
        (x,y).Dump()
        f x y
let fromStreams s streams =
    streams
    |> Seq.collect id
    |> Seq.fold (foldHooker InventoryItem.Apply) s
//events
//|> Seq.fold (foldHooker InventoryItem.Apply) InventoryItem.Zero
[events]
|> fromStreams InventoryItem.Zero
|> Dump
|> ignore
