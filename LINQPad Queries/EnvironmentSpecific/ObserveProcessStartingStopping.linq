<Query Kind="FSharpProgram" />

//poll pids, observe, with kill option
// WIP: drowning in complexity? 
let dumpt (t:string) x = x.Dump(t) |> ignore; x
module Killing =
    let link title pid =
        Hyperlinq(Action(fun () -> Process.GetProcessById(pid).Kill()), sprintf "kill %s, pid:%i" title pid)
    
    (
    let p = Process.GetCurrentProcess()
    link p.ProcessName p.Id
    |> Dump
    |> ignore
    )

let processNameBlackList = [
    "chrome";"Idle";"System";"AdaptiveSleepService";"services"
    // added because of access denied:
    "csrss"; "svcHost"; "sqlwriter";"StandardCollector.Service"
    ]
let highlight = ["devenv"; ]

type LoopStyle =
    | Run
    | Stop
    | Pause
    
let mutable looping = LoopStyle.Run
let updateLoopingStatus =
    let dc = DumpContainer()
    dc.Dump("runStatus")
    dc.Content <- sprintf "%A" looping
    Hyperlinq(Action(fun() -> 
            match looping with 
            | Run -> looping <- LoopStyle.Pause 
            | Pause -> looping <- LoopStyle.Run 
            | _ -> ()
            dc.Content <- sprintf "%A" looping
            
        ), "Toggle Pause").Dump()
    let stopAction() =
        looping <- LoopStyle.Stop
        dc.Content <- sprintf "%A" looping
    Hyperlinq(Action(stopAction),"Stop").Dump()
    ()
type AccessResult<'T> =
    | Success of 'T
    | Failure of exn
    with 
        member x.Iter f : unit = f x
        member x.Bind f = match x with | Success x -> f x | Failure exn -> Failure exn
        member x.Map f = match x with | Success x -> f x |> Success | x -> x
        member x.GetOr y = match x with | Success x -> x | Failure _ -> y

let getValueOpt f x =
        try
            f x |> Success
        with ex ->
            Failure ex
let alreadyDumpedItems = ResizeArray<int*string>()            
let dumpLater = ResizeArray<string*obj>()
type System.Diagnostics.Process with
    member x.TryGetOrDumpWithName f =
        x 
        |> getValueOpt f
        |> function
            | Success x -> Some x
            | Failure exn ->
                if alreadyDumpedItems |> Seq.contains (x.Id,x.ProcessName) |> not then
                    alreadyDumpedItems.Add(x.Id,x.ProcessName)
                    // assumes we can access process name
                    //(x,exn).Dump(x.ProcessName)
                    dumpLater.Add(x.ProcessName,upcast (x,exn))
                None
    //member x.IdTryGet() = x.TryGetOrDumpWithName (fun x -> x.Id)
//    member x.ProcessNameTryGet() =
//        x.TryGetOrDumpWithName (fun x -> x.ProcessName)
    member x.StartTimeTryGet() = x.TryGetOrDumpWithName (fun x -> x.StartTime)
    member x.HasExitedTryGet() = x.TryGetOrDumpWithName (fun x -> x.HasExited)
    

let updateProcessContainer =
    let makeDisplay (p:Process) = 
        // we're assuming if we get into the display part, that property access works
        // p.HasExitedTryGet() |> (fun x -> x.GetOr false)
        match p.HasExitedTryGet() with
        | Some true -> box p.ProcessName
        | Some false -> upcast Killing.link p.ProcessName p.Id
        | None -> box p.ProcessName

    let buildProcessList (items: Process seq) = 
        items 
        |> Seq.filter (fun p -> processNameBlackList |> Seq.exists (stringEqualsI p.ProcessName) |> not) 
        |> List.ofSeq
    let buildAlreadyMap (items: Process seq) =
        items
        |> Seq.choose (fun p ->
            p.HasExitedTryGet()
            |> Option.map (fun hasExited -> p.Id,hasExited)
        )
        //|> Seq.map (fun p -> p.Id, p.HasExited) 
        |> Map.ofSeq
    
    // hold processes that were running at startup            
    
    let alreadyRunningProcesses = Process.GetProcesses() |> buildProcessList
    // processes that Win32Exception trying to access certain properties
    let mutable alreadyRunningHasExitedMap = alreadyRunningProcesses |> buildAlreadyMap
    alreadyRunningProcesses.Count().Dump("alreadyRunning count")
    let hasExitChanged (p:Process) = alreadyRunningHasExitedMap |> Map.containsKey p.Id && p.HasExited && not <| alreadyRunningHasExitedMap.[p.Id]
    let updateAlready,updateLater =
        let getAlreadyDisplay () = 
            alreadyRunningProcesses
            |> Seq.map makeDisplay
            |> List.ofSeq
        let alreadyRunning = DumpContainer()
        alreadyRunning.Style <- "float:right"
        let openedLater = DumpContainer()
        //openedLater.Style <- "float:right"
        // failedTo Horizontal: Util.HorizontalRun(false, alreadyRunning, openedLater).Dump()
        alreadyRunning.Dump()
        openedLater.Dump("openedLater")
//        alreadyRunning.Style.Dump()
//        openedLater.Style.Dump()
        (fun () -> 
            if Debugger.IsAttached then
                Debugger.Break()
            alreadyRunning.Content <- getAlreadyDisplay()
        ), (fun (p:Process list) ->
            openedLater.Content <- p |> List.map makeDisplay
        )
        
    let f () = 
        let processes = 
            Process.GetProcesses()
            |> Seq.filter(fun p -> processNameBlackList |> Seq.exists (stringEqualsI p.ProcessName) |> not)
            |> Seq.sortBy(fun p -> p.ProcessName)
            |> List.ofSeq
        // remove items that are no longer running
        let processesAreEqual (p:Process) (p2:Process) =
            p2.Id = p.Id && p2.ProcessName = p.ProcessName && p2.StartTimeTryGet() = p.StartTimeTryGet()

        let processHasExited = alreadyRunningProcesses |> Seq.exists hasExitChanged
        if processHasExited then
            alreadyRunningHasExitedMap <- alreadyRunningProcesses |> buildAlreadyMap
            updateAlready()
        processes
        |> Seq.filter(fun p -> alreadyRunningProcesses |> Seq.exists (processesAreEqual p))
        |> List.ofSeq
        |> updateLater
        ()
    //f()
    f
let token = Util.KeepRunning()
let runLoop () = 
    let mutable hasLooped = false
    let mutable hasDumped = false
    try
        while (match looping with | Stop -> false | _ -> true) do
            if not hasDumped && hasLooped then
                hasDumped <- true
                Util.OnDemand("access denied failures", Func<_>(fun () -> dumpLater |> Seq.sortBy fst |> List.ofSeq)).Dump()
                
            match looping with
            | Run ->
                updateProcessContainer()
            | _ -> ()
            hasLooped <- true
            System.Threading.Thread.Sleep(1500)
    with ex ->
        ex.Dump("Thread exception, stopping")
    token.Dispose()
let t = System.Threading.Thread(runLoop)
t.Start()
