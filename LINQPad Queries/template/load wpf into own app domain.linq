<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\PresentationCore.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\PresentationFramework.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\System.Windows.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\System.Xaml.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.2\WindowsBase.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <Namespace>System.Windows</Namespace>
  <Namespace>System.Windows.Controls</Namespace>
  <Namespace>System.Windows.Data</Namespace>
</Query>

// start a wpf in own app domain
// see: https://stackoverflow.com/questions/23454023/start-wpf-app-from-unit-test-in-its-own-appdomain

[<Serializable>]
type MyMarshal() =
    inherit MarshalByRefObject()
    let ka = Util.KeepRunning()
    member __.Dispose() = ka.Dispose()
    member __.Print s = printfn "%s" s
type IChild =
    abstract member Initialize : MyMarshal -> unit
    
[<Serializable>]
type Child() =
    inherit MarshalByRefObject()
    let mutable mm = None
    member __.Initialize (mmToSet:MyMarshal) = 
        mm <- Some mmToSet 
    interface IChild with
        member x.Initialize( mm) = x.Initialize(mm)
    
let startApp (mm:MyMarshal) () = 
    let a = System.Windows.Application()
    let w = Window()
    w.Content <- 
        (
            let sp = StackPanel()
            sp.Children.Add(Label(Content="Hello world")) |> ignore
            sp
        )
    mm.Print "starting"
    a.Run(w)
    |> ignore<int>
    mm.Print "Started?"
let makeAddHandlerDisposable fAdd fRemove fDelegate =
    fAdd fDelegate
    { new IDisposable with
        member __.Dispose() = 
            fRemove fDelegate
    }
let assm = lazy(System.Reflection.Assembly.GetExecutingAssembly())
let aar:ResolveEventHandler = 
    ResolveEventHandler(fun sender e ->
        MessageBox.Show(sprintf "trying to resolve something %s" e.Name) |> ignore
        assm.Value.GetLoadedModules()
        |> Seq.tryFind(fun m -> m.Name = e.Name)
        |> function
            | Some m ->
                m.Assembly
            | None ->
                MessageBox.Show(sprintf "boooooo %s" e.Name) |> ignore
                e.Dump()
                null
            
    )
let ownDomain f =
    // let ad = AppDomain.CreateDomain("WpfTest", AppDomain.CurrentDomain.Evidence, AppDomain.CurrentDomain.SetupInformation)
    // https://stackoverflow.com/questions/4343845/could-not-load-file-or-assembly
    let ad = Util.CreateAppDomain("WpfTest", AppDomain.CurrentDomain.Evidence, AppDomain.CurrentDomain.SetupInformation)
    let disp = makeAddHandlerDisposable ad.add_AssemblyResolve ad.remove_AssemblyResolve aar
    printfn "Starting app domain get assemblies and force load loop"
//    assm.Value.GetReferencedAssemblies()
    AppDomain.CurrentDomain.GetAssemblies()
    |> Seq.iter(fun m ->
        printfn "Trying to load %s" m.CodeBase
        try
            ad.Load (m.FullName) |> ignore
            printfn "Loaded into AppDom %s" m.FullName
        with ex ->
            printfn "Failing to load %s" m.FullName
            m.Dump()
    )
    
    printfn "Started up"
    let mm = MyMarshal()
    mm.Print "Testing mm"
    let child = 
        try
            ad.CreateInstanceFromAndUnwrap(assm.Value.CodeBase, typeof<Child>.FullName) 
        with _ -> 
            reraise()
    printfn "made a baby"
    try
        printfn "AppDomain delegates hooking"
        let disp = makeAddHandlerDisposable ad.DomainUnload.AddHandler ad.DomainUnload.RemoveHandler (EventHandler(fun _ _ -> printfn "weee?"))
        ad.DoCallBack(CrossAppDomainDelegate (f mm))
        printfn "Finished app, closing?"
        AppDomain.Unload ad
        mm.Dispose()
    with ex ->
        printfn "exceptional"
        mm.Dispose()
        reraise()
    printfn "Moving on with life"
    
//StartApp()
ownDomain startApp

