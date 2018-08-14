<Query Kind="FSharpProgram" />

// watch directory(ies) with file hash

open System.Security.Cryptography
open System.Collections.ObjectModel
        
let toWatch = [
    @"C:\ProgramData\Accelerated Payment Technologies\XLCloud"
]
let clickableDirLink x = 
    LINQPad.Util.RawHtml(sprintf "<a href=\"%s\" title=\"%s\">link</a>" x x).Dump(x)

let tryOrDump f = 
    try
        f()
    with ex ->
        ex.Dump()
type AggregateDisposable() =
    let items = ResizeArray()
    member __.Add(x:IDisposable) =
        items.Add x
    member __.Dispose() =
            items
            |> Seq.iter(fun x -> tryOrDump x.Dispose)
    new (x) as this =
        new AggregateDisposable()
            then 
                this.Add x
    interface IDisposable with
        member x.Dispose() = x.Dispose()
            
let hashFile fullPath = 
    use md5 = MD5.Create()
    use s = File.OpenRead fullPath
    //hash
    let has = md5.ComputeHash s    
    // make it into a readable string
    BitConverter.ToString(has).Replace("-","").ToLowerInvariant()
    
let getEnumName<'T> x =
    Enum.GetName(typeof<'T>, x)
let getHashes p = 
    Directory.GetFiles(p,"*",SearchOption.AllDirectories)
    |> Array.map(fun x -> x,hashFile x)
type Rename = {OldName:string;NewName:string}
type FileChange =
    |Deleted
    |Changed of hash:string
    |Created of hash:string
    |Renamed of oldFullPath:string option
    
let watch (obs:ObservableCollection<_>,aDisp:AggregateDisposable) dirPath =
    let addDisp x = 
        x :> IDisposable
        |> aDisp.Add
    let withChange (args:FileSystemEventArgs) =
        let add x = (args.Name, args.FullPath, x) |> obs.Add
        match args.ChangeType,args with
        | WatcherChangeTypes.Deleted,_ -> 
            add FileChange.Deleted
        | WatcherChangeTypes.Created,_ ->
            hashFile args.FullPath
            |> Created
            |> add
        | WatcherChangeTypes.Changed,_ ->
            hashFile args.FullPath
            |> Changed
            |> add
        | WatcherChangeTypes.Renamed, ( :? RenamedEventArgs as args) ->
            args.OldFullPath
            |> Some
            |> Renamed
            |> add
        | WatcherChangeTypes.Renamed, _ ->
            None
            |> Renamed
            |> add
        | _ ->
            args.Dump("Unaccounted for change %A")
        
    if not <| Directory.Exists dirPath then raise <| DirectoryNotFoundException(dirPath)
    LINQPad.Util.RawHtml(sprintf "<a href=\"%s\" title=\"%s\">link</a>" dirPath dirPath).Dump(dirPath)
    let fs = new FileSystemWatcher(dirPath,IncludeSubdirectories=true,EnableRaisingEvents=true)
//    disp <- (fs :> IDisposable)@disp
    addDisp fs
    fs.Changed.Subscribe withChange |> addDisp
    obs,aDisp

let dc = DumpContainer()
dc.Dump()
toWatch
|> List.map (fun x -> clickableDirLink x; x)
// get/dump all hashes grouped by toWatch key
|> Seq.fold(fun (items,hashes) x -> x::items, (x,getHashes x):: hashes) (List.empty,List.empty)
|> fun (x, hashes) -> hashes.Dump("hashes"); x
|> List.ofSeq
|> List.rev
|> Seq.fold watch (ObservableCollection(), new AggregateDisposable(Util.KeepRunning()))
|> fun (obs,aDisp) ->
    obs.CollectionChanged.Subscribe(fun _ -> 
        dc.Content <- null
        dc.Content <- obs
    )
    |> aDisp.Add
    Util.ReadLine()
    |> ignore
    aDisp.Dispose()