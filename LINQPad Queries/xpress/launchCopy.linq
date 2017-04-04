<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

// launch a copy of xpress (without a debugger attached)

let after (delimiter:string) (text:string) = text.Substring(text.IndexOf(delimiter) + delimiter.Length)
let before (delimiter:string) (text:string) = text.Substring(0,text.IndexOf(delimiter))
let dumpt (title:string) x = x.Dump(title); x
let linq t f = Hyperlinq(Action(f),t)
let dumpLength title items = 
    Seq.length items |> dumpt title |> ignore
    items

module PInvoke = 
    type EnumThreadDelegate= delegate of (IntPtr * IntPtr) -> bool
    
    [<DllImport("USER32.DLL")>]
    extern bool private SetForegroundWindow(IntPtr hWnd)
    
    //[<DllImport("user32.dll", CharSet=CharSet.Auto, SetLastError=true)>]
    //extern bool private EnumWindows(EnumThreadDelegate callback, IntPtr extraData);
    
    [<DllImport("user32.dll")>]
    extern [<return: MarshalAs(UnmanagedType.Bool)>] bool private EnumThreadWindows(int dwThreadId, EnumThreadDelegate lpfn, IntPtr lParam);
    
    let setForegroundWindow hWnd = SetForegroundWindow hWnd
//    let enumWindows processId = // http://stackoverflow.com/questions/2531828/how-to-enumerate-all-windows-belonging-to-a-particular-process-using-net
//        Process.GetProcessById(processId).Threads
//        |> Seq.cast<ProcessThread>
//        |> Seq.map(fun pt -> 
//            //let items = List<IntPtr>()
//            let getItems (hWnd:IntPtr, _lParam:IntPtr) = 
//                //items <- hWnd :> obj |> (fun h -> (List.ofArray items |> fun l -> h::l) |> Array.ofList)
//                //items.Add(hWnd)
//                true
//            EnumThreadWindows(pt.Id, EnumThreadDelegate(getItems),IntPtr.Zero) |> ignore
//            pt.Id,items
//            )
    
    
// delete clutter/unnecessary files

type DeleteStrategy = |Tidy |FullClean |Obliterate

let getShouldDelete strategy = 
    match strategy with
    | Obliterate -> id
    | _ ->     
        Seq.choose (fun f ->
            match Path.GetExtension f with
            | ".log"
            | ".txt" ->
                match strategy with 
                | Tidy -> 
                    let dt = File.GetLastWriteTime f                
                    let age = DateTime.Now - dt
                    if age.TotalDays > 1. then
                        Some f
                    else None
                | FullClean |Obliterate -> Some f
                
            | _ -> None
           )

let getShouldRemove =
    Seq.choose(fun d ->
        Some d
    )
    
let clean strategy dir = 
    Directory.GetFiles dir
    |> getShouldDelete strategy
    |> Array.ofSeq
    //|> dumpt "deleting"
    //|> dumpLength "to delete"
    |> Seq.iter File.Delete
    
    Directory.GetDirectories dir
    |> getShouldRemove (* on all strategies, removing all directories is advisable *)
    //|> dumpt "deleting"
    |> Seq.iter(fun d -> Directory.Delete(d,true))

let copy s d = 
    Directory.GetFiles(s,"*",SearchOption.AllDirectories)
    |> Seq.map (fun fn -> fn, Path.Combine(d,fn |> after s))
    //|> dumpt "to copy"
    |> Seq.iter (fun (sf,df) -> File.Copy(sf,df))
    |> ignore
    
let dumpThreadInfo (t:string) = 
    System.Threading.Thread.CurrentThread.ManagedThreadId.Dump(t)    
    System.Threading.Thread.CurrentThread.Dump(t)

let updateLaunchedProcesses (p:Process) = 
    let pId = p.Id
    let key = "launchedProcesses"
    let previouslyLaunched = 
        match AppDomain.CurrentDomain.GetData key with 
        | null -> List.Empty 
        | x -> x :?> int list
    let currentProcesses = 
        Process.GetProcesses()
        |> Seq.filter(fun p -> previouslyLaunched |> Seq.contains p.Id)
        |> Seq.filter(fun p -> p.StartInfo.FileName.Contains("PracticeManagement"))
        |> Seq.toList
        |> List.map(fun p -> p.Id)
    
    let updatedP=
        previouslyLaunched
        |> List.filter(fun pId -> currentProcesses |> Seq.contains pId)
        |> fun x -> pId::x
    AppDomain.CurrentDomain.SetData(key, updatedP)
    updatedP.Dump("Pm processes")
    p
    
let launch targetDir exe = 
    dumpThreadInfo "launching process on"
    let fullPath = Path.Combine(targetDir,exe)
    let psi = 
        fullPath
        |> ProcessStartInfo
    fullPath.Dump()
    psi.RedirectStandardError <- true
    psi.RedirectStandardOutput <- true
    psi.UseShellExecute <- false
    psi.WorkingDirectory <- targetDir
    
    psi
    |> Process.Start
    |> fun p -> p.EnableRaisingEvents <- true; p
    |> (fun p -> p.ErrorDataReceived.Add (fun ed -> ed.Dump(); printfn "error: %A" ed.Data); p)
    |> (fun p -> p.OutputDataReceived.Add (fun ed -> ed.Dump(); printfn "output: %A" ed.Data); p)
    |> (fun p -> p.Exited.Add (fun _ -> "exited".Dump(); p.Dump()); p)
    |> updateLaunchedProcesses
    

let sourceDir = @"C:\TFS\PracticeManagement\dev\PracticeManagement\bin\"
let targetDir = @"C:\TFS\PracticeManagement\dev\PracticeManagement\bin2\"
    
clean Tidy sourceDir 
clean Obliterate targetDir
copy sourceDir targetDir
dumpThreadInfo "main threadId"
//System.Threading.Thread.CurrentThread.ManagedThreadId.Dump("main threadId")
let onThread (title:string) f () = 
    let t = Threading.Thread(ThreadStart((fun _ -> 
                    dumpThreadInfo title
                    try
                        f()
                    with ex -> ex.Dump()
                        )))
    t.Start()
let mutable p: Process = null
let runner = Util.KeepRunning()
//let mainThread = System.Threading.Thread.CurrentThread
let thread = 
    Threading.Thread(ThreadStart((fun _ -> 
        dumpThreadInfo "ThreadStart thread"
        //try
        try
                p <- launch targetDir "PracticeManagement.exe"  
                [ 
                    (sprintf "kill %i" p.Id), (fun () -> p.Kill())
                    "sleep", (fun () -> 
                        dumpThreadInfo "sleep thread"
                        System.Threading.Thread.Sleep(1000))
                    "getWaitReasons", (fun () -> 
                        Process.GetProcessById(p.Id).Threads
                        |> Seq.cast<ProcessThread>
                        |> Seq.map(fun t -> t.Id, t.WaitReason)
                        |> Dump
                        |> ignore)
                    "getProcess", (fun () ->     
                                            dumpThreadInfo "getProcessOnThread threadId"
                                            p.Dump())
                    "getProcessOnThread", onThread "getProcessOnThread" (fun () -> 
                                            dumpThreadInfo "getProcessOnThread threadId"
                                            p.Dump())
                    
                    "toForeground", (fun () -> PInvoke.setForegroundWindow p.MainWindowHandle |> ignore<bool>)
                    "getMainWindowTitle", (fun () -> p.MainWindowTitle.Dump())
                    "getMainWindowHandle", (fun () -> p.MainWindowHandle.Dump())
//                    "getWindows", (fun () ->
//                        try
//                            PInvoke.enumWindows p.Id |> dumpt "enumWindows!" |> ignore
//                        with ex -> ex.Dump()
//                    )
                    "getWindow", (fun () -> 
                        dumpThreadInfo "getWindow threadId"
                        try
                            let x = 
                                System.Windows.Interop.HwndSource.FromHwnd p.MainWindowHandle 
                            x|> dumpt "hwndSource!" |> ignore
                            //let w = x :?> System.Windows.Window
                            //w.RootVisual.Dump()
                        with ex -> ex.Dump()
                            )   
                ]
                |> Seq.map (fun (t,a) -> linq t a)
                |> Dump
                |> ignore

                //System.Windows.Interop.HwndSource.FromHwnd p.MainWindowHandle |> dumpt "hwndSource!" |> ignore
                //p.WaitForExit()
                //Util.ReadLine() |> ignore
        with ex -> ex.Dump("thread exception!")
//        finally
//            runner.Dispose()
    )))
thread.SetApartmentState( ApartmentState.STA)
thread.Start()

System.Threading.Thread.Sleep(5000)
runner.Dispose()

//System.Threading.Thread.Sleep(1000)
//while not p.HasExited do
//    System.Threading.Thread.Sleep(600)
//runner.Dispose()