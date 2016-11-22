namespace Pm.Schema

open System
open System.Runtime.CompilerServices
open Pm.Schema.BReusable
open System.Collections.Generic

[<System.Runtime.CompilerServices.Extension>]
module CHelpers =

//    // useful for returning a value that was explicitly set to null vs unset
//    let SomeNull<'t when 't:null>() : 't option = Some null

    [<Extension>]
    let After s delimiter = s |> after delimiter
    [<Extension>]
    let Before s delimiter = s|> before delimiter
    [<Extension>]
    let EqualsI s1 s2 = s1 |> String.equalsI s2
    [<Extension>]
    let BeforeOrSelf s delimiter = if containsI delimiter s then s |> before delimiter else s
    [<Extension>]
    let Delimit (source:IEnumerable<string>) delimiter = source |> delimit delimiter // String.Join(delimiter,source |> Array.ofSeq)
    [<Extension>]
    let IsNullOrEmpty= String.IsNullOrEmpty
    [<Extension>]
    let GetValueOrDefault (x:_ option) = 
        match x with
            | Some x -> x
            | None -> Unchecked.defaultof<_>
    [<Extension>]
    let DisposeIfNotNullAndDisposable (o:obj) = 
        match o with
        | null -> ()
        | :? IDisposable as d -> d.Dispose()
        | _ -> ()

    [<Extension>]
    let ToOptionFromClass = function | null -> None | x -> Some x

    // useful for passing unset vs set to null vs a value
    // consider alternate implementation :
    //let ToSome<'t when 't : null>(x:'t) = Some x
    [<Extension>]
    let ToSome x = Some x
    // set None/null option to Some(null) because it was explicitly set to null, not unset.
    [<Extension>]
    let ToSomeNull<'t when 't:null>(_:'t option) : 't option = Some null

    [<Extension>]
    let Map(x: _ option) (f:Func<_,_>) = 
        x
        |>Option.map (f.Invoke)

    [<Extension>]
    let GetOrDefault (x:_ option) defaultValue =
        match x with
        | Some x -> x
        | None -> defaultValue

    [<Extension>]
    let ToNullable(x:_ option) = Option.toNullable x

    [<Extension>]
    let IsNone (x:_ option) =
        match x with
        | Some _ -> false
        | None -> true

    [<Extension>]
    let IsSome (x:_ option) =
        match x with
        | Some _ -> true 
        | None -> false 

    [<Extension>]
    let CreateOption<'t when 't:struct>(x: 't) = Some x

    [<Extension>]
    let ToOption(x: _ Nullable) = 
        match x with
        |NullableNull -> None
        |NullableValue x -> Some x

    // move this to breusable, and perhaps link this extension to calling it
    // taken from SO http://stackoverflow.com/a/1595311/57883
    [<Extension>]
    let GetAge(dob:DateTime, now:DateTime) =
        let age = now.Year - dob.Year
        if now.Month < dob.Month || (now.Month = dob.Month && now.Day < dob.Day) then 
            age - 1
        else
            age

    // move this to breusable, and perhaps link this extension to calling it
    [<Extension>]
    let GetAgeInMonths (dob:DateTime, now:DateTime) = ((now.Year - dob.Year) * 12) + now.Month - dob.Month

//    let GetXmlDoc (element:System.Xml.Linq.XElement) = 
//    public static XmlDocument GetXmlDoc(this XElement element)
//    {
//        using (XmlReader xmlReader = element.CreateReader())
//        {
//            var xmlDoc = new XmlDocument();
//            xmlDoc.Load(xmlReader);
//            return xmlDoc;
//        }
//    }

// http://blogs.msdn.com/b/jaredpar/archive/2010/07/27/converting-system-func-lt-t1-tn-gt-to-fsharpfunc-lt-t-tresult-gt.aspx
[<Extension>]
type public FSharpFuncUtil =

    [<Extension>]
    static member ToFSharpFunc(func:Func<'a>) =  func.Invoke

    [<Extension>] 
    static member ToFSharpFunc<'a,'b> (func:Converter<'a,'b>) = func.Invoke

    [<Extension>] 
    static member ToFSharpFunc<'a,'b> (func:Func<'a,'b>) = fun x -> func.Invoke x

    [<Extension>] 
    static member ToFSharpFunc<'a,'b,'c> (func:Func<'a,'b,'c>) = fun x y -> func.Invoke (x,y)

    [<Extension>] 
    static member ToFSharpFunc<'a,'b,'c,'d> (func:Func<'a,'b,'c,'d>) = fun x y z -> func.Invoke (x,y,z)

    [<Extension>]
    static member ToFSharpFunc<'a,'b,'c,'d,'e>(func:Func<'a,'b,'c,'d,'e>) = fun a b c d -> func.Invoke(a,b,c,d)

    [<Extension>]
    static member ToFSharpAct (act: Action) = fun () -> act.Invoke()

    [<Extension>] 
    static member ToFSharpAct<'a> (act: Action<'a>) = fun x -> act.Invoke(x)

    [<Extension>] 
    static member ToFSharpAct<'a,'b> (act: Action<'a,'b>) = fun x y -> act.Invoke(x,y)

    [<Extension>]
    static member ToFSharpAct<'a,'b,'c> (act: Action<'a,'b,'c>) = fun x y z -> act.Invoke(x,y,z)

    [<Extension>]
    static member ToFsharpAct<'a,'b,'c,'d> (act: Action<'a,'b,'c,'d>) = fun a b c d-> act.Invoke(a,b,c,d)

    static member Create<'a,'b> (func:Func<'a,'b>) = FSharpFuncUtil.ToFSharpFunc func

    static member Create<'a,'b,'c> (func:Func<'a,'b,'c>) = FSharpFuncUtil.ToFSharpFunc func

    static member Create<'a,'b,'c,'d> (func:Func<'a,'b,'c,'d>) = FSharpFuncUtil.ToFSharpFunc func


type private SimpleMonitor() =
    let mutable busyCount = 0
    member __.Enter() = 
        busyCount <- busyCount + 1
    member __.Dispose() =
        busyCount <- busyCount - 1

    member __.Busy with get() = busyCount > 0

    interface System.IDisposable with
        member x.Dispose() = x.Dispose()

//// WIP: untested, unused.
//module ReObservable = 
//    open System.ComponentModel
//    open System.Collections.Specialized
//    type CollectionChangedDelegate = 
//            delegate of 
//                sender:obj * e:NotifyCollectionChangedEventArgs -> unit
//    /// Initializes a new instance of ObservableCollection that is empty and has default initial capacity.
//    /// translated from http://referencesource.microsoft.com/#System/compmod/system/collections/objectmodel/observablecollection.cs
//    type ReObservableCollection<'t>(list: 't List) as self = // http://referencesource.microsoft.com/#System/compmod/system/collections/objectmodel/observablecollection.cs
//        inherit System.Collections.ObjectModel.Collection<'t>(if not <| isNull list then List<_>(list.Count) else list)
//            do
//                if not<| isNull list then
//                    self.copyFrom list
//        let collectionChanged = Event< _ , _ > ( )
//        let propertyChangedEvent = new DelegateEvent<PropertyChangedEventHandler>()
//        let monitor = new SimpleMonitor()
//
//        static let IndexerName = "Item[]"
//
//        new () = ReObservableCollection(null)
//        new (collection : _ seq) as self =
//            ReObservableCollection(null)
//                then
//                    if isNull collection then
//                        raise <| System.ArgumentNullException("collection")
//                        self.copyFrom collection
//
//        member x.Move oldIndex newIndex = x.MoveItem (oldIndex, newIndex)
//        override x.ClearItems() =
//            x.CheckReentrancy ()
//            base.ClearItems ()
//            x.OnPropertyChanged "Count"
//            x.OnPropertyChanged IndexerName
//            x.OnCollectionReset ()
//
//        override x.RemoveItem (index:int) = 
//            x.CheckReentrancy ()
//            let removedItem = x.Item index
//            base.RemoveItem index
//            x.OnPropertyChanged "Count"
//            x.OnPropertyChanged IndexerName
//            x.OnCollectionChanged (NotifyCollectionChangedAction.Remove, removedItem, index)
//
//        override x.InsertItem (index, item) = 
//            x.CheckReentrancy()
//            base.InsertItem (index, item)
//
//        override x.SetItem (index, item) = 
//            x.CheckReentrancy ()
//            let originalItem = x.Item index
//            base.SetItem(index, item)
//
//            x.OnPropertyChanged IndexerName
//            x.OnCollectionChanged (NotifyCollectionChangedAction.Replace, originalItem, item, index)
//
//        member private x.MoveItem (oldIndex, newIndex) = 
//            x.CheckReentrancy ()
//            let removedItem = x.[oldIndex]
//            base.RemoveItem oldIndex
//            base.InsertItem(newIndex, removedItem)
//            x.OnPropertyChanged IndexerName
//            x.OnCollectionChanged(NotifyCollectionChangedAction.Move, removedItem, newIndex, oldIndex)
//
//        member private x.OnPropertyChanged (e:PropertyChangedEventArgs) = propertyChangedEvent.Trigger [| x;e |]
//
//        member private x.OnPropertyChanged name = 
//            PropertyChangedEventArgs name
//            |> x.OnPropertyChanged
//
//        member private x.OnCollectionChanged (e:NotifyCollectionChangedEventArgs) = 
//            use __ = x.BlockReentrancy()
//            collectionChanged.Trigger(x, e)
//
//        member private x.OnCollectionChanged (action, item, index:int) =
//            NotifyCollectionChangedEventArgs(action, item, index)
//            |> x.OnCollectionChanged
//
//        member private x.OnCollectionChanged (action, item, index:int, oldIndex:int) =
//            NotifyCollectionChangedEventArgs(action, item, index, oldIndex)
//            |> x.OnCollectionChanged
//
//        member private x.OnCollectionChanged (action, oldItem:obj, newItem:obj, index:int) =
//            NotifyCollectionChangedEventArgs(action, newItem, oldItem, index)
//            |> x.OnCollectionChanged
//
//        member x.OnCollectionReset() =
//            NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset)
//            |> x.OnCollectionChanged
//
//        member private __.BlockReentrancy () = 
//            monitor.Enter () 
//            monitor :> IDisposable
//
//        member private x.CheckReentrancy () = 
//            if monitor.Busy then
//                let _inotif = x :> INotifyCollectionChanged
//
//                // desired: block reentrancy
//                //let _x = inotif :> System.MulticastDelegate
//                //let multicast = inotif.CollectionChanged :> System.MulticastDelegate
//                //if not <| isNull inotif.CollectionChanged. && collectionChanged.GetInvocationList().Length > 1 then
//                //raise <| InvalidOperationException("Reentrancy is not allowed")
//                ()
//        member private x.copyFrom (collection : _ seq ) =
//            let items = x.Items
//            if not <| isNull collection && not <| isNull items then
//                use enumerator = collection.GetEnumerator()
//                while enumerator.MoveNext() do
//                    items.Add enumerator.Current
//
//        interface INotifyCollectionChanged with
//            [<CLIEvent>]
//            member __.CollectionChanged = collectionChanged.Publish
//
//        interface INotifyPropertyChanged with
//            [<CLIEvent>]
//            member __.PropertyChanged = propertyChangedEvent.Publish