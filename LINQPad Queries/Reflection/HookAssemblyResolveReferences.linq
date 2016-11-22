<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Globalization.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Namespace>Microsoft.FSharp.Reflection</Namespace>
  <Namespace>System.Data.OleDb</Namespace>
  <Namespace>System.Windows.Forms</Namespace>
</Query>

// always run in a new app domain ?
//Util.NewProcess <- true

printfn "starting up"
printfn "newProcess is %A" Util.NewProcess
let listenToResolveAttempts () = 
    let onResolveEvent = new ResolveEventHandler( fun sender evArgs ->
        //let requestedAssembly = AssemblyName(evArgs.Name)
        printfn "Found resolve event"
        try
            let requestingAssembly = if not <| isNull evArgs.RequestingAssembly then evArgs.RequestingAssembly.CodeBase else "null"
            printfn "requested assembly:%s by %s" evArgs.Name requestingAssembly
        with ex -> ex.Dump()
        null
        )
    AppDomain.CurrentDomain.TypeResolve.Add (fun e -> e.)
    AppDomain.CurrentDomain.add_AssemblyResolve onResolveEvent
    
let RedirectAssembly shortName (targetVersion:Version) publicKeyToken =
    
    let rec onResolveEvent = new ResolveEventHandler( fun sender evArgs ->
        let requestedAssembly = AssemblyName(evArgs.Name)
        if requestedAssembly.Name <> shortName then
            Unchecked.defaultof<Assembly>
        else
            printfn "Redirecting assembly load of %s ,\tloaded by %s" evArgs.Name (if evArgs.RequestingAssembly = null then "(unknown)" else evArgs.RequestingAssembly.FullName)
            requestedAssembly.Version <- targetVersion
            requestedAssembly.SetPublicKeyToken(AssemblyName(sprintf "x, PublicKeyToken=%s" publicKeyToken).GetPublicKeyToken())
            requestedAssembly.CultureInfo <- System.Globalization.CultureInfo.InvariantCulture
            AppDomain.CurrentDomain.remove_AssemblyResolve(onResolveEvent)
            Assembly.Load(requestedAssembly)
            )
    AppDomain.CurrentDomain.add_AssemblyResolve(onResolveEvent)
 
 
let test = 
    listenToResolveAttempts()
    System.Threading.Thread.Sleep(1000)
    //try
    Assembly.Load("WindowsBase") |> ignore<Assembly>
    //with ex -> ex.Dump()
    "finished".Dump()
    //AppDomain.CurrentDomain.Load("WindowsBase")

//AppDomain.Unload(AppDomain.CurrentDomain)