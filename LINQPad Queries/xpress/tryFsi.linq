<Query Kind="FSharpExpression">
  <Reference>&lt;RuntimeDirectory&gt;\ISymWrapper.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\MSBuild\14.0\Bin\Microsoft.Build.Framework.dll</Reference>
  <NuGetReference>FSharp.Compiler.Service</NuGetReference>
  <Namespace>Microsoft.FSharp.Compiler.Interactive</Namespace>
  <Namespace>Microsoft.FSharp.Compiler.Interactive.Shell</Namespace>
</Query>

// fails in linqpad
AppDomain.CurrentDomain.GetAssemblies().Dump()

let noRedirect sender (evArgs:ResolveEventArgs) =
    printfn "Not redirecting %A, requestor: %s" evArgs.Name (if evArgs.RequestingAssembly = null then "(unknown)" else evArgs.RequestingAssembly.FullName)
    Unchecked.defaultof<Assembly>

let tryRedirect shortName targetVersion publicKeyToken beforeLoadF (sender:obj) (evArgs:ResolveEventArgs)   =
    let requestedAssembly = AssemblyName(evArgs.Name)
    if requestedAssembly.Name <> shortName then
        noRedirect sender evArgs
    else
        printfn "Redirecting assembly load of %s ,\tloaded by %s" evArgs.Name (if evArgs.RequestingAssembly = null then "(unknown)" else evArgs.RequestingAssembly.FullName)
        requestedAssembly.Version <- targetVersion
        requestedAssembly.SetPublicKeyToken(AssemblyName(sprintf "x, PublicKeyToken=%s" publicKeyToken).GetPublicKeyToken())
        requestedAssembly.CultureInfo <- System.Globalization.CultureInfo.InvariantCulture
        match beforeLoadF with
        | Some f -> 
            f ()
        | None -> ()
        Assembly.Load(requestedAssembly)

let redirectAssembly shortName (targetVersion:Version) publicKeyToken =
    let onResolveEvent = new ResolveEventHandler( tryRedirect shortName targetVersion publicKeyToken None)
    AppDomain.CurrentDomain.add_AssemblyResolve onResolveEvent
do 
    let microsoftKey = "b03f5f7f11d50a3a"
    redirectAssembly "FSharp.Core" (Version("4.3.0.0")) microsoftKey

let fsharpCorePath = @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll"
try
    System.Reflection.Assembly.GetEntryAssembly().GetReferencedAssemblies().Dump("referenced assemblies")
with _ -> System.Reflection.Assembly.GetEntryAssembly().Dump("entry assembly")
System.Threading.Thread.Sleep(100)
try
    // force load of fsi

    use out = new StringWriter ()
    use err = new StringWriter ()
    let cfg = FsiEvaluationSession.GetDefaultConfiguration ()
    ()
    use fsi = FsiEvaluationSession.Create (cfg, [|"--noninteractive"; "-r"; fsharpCorePath |], new StringReader (""), out, err)
    fsi.Dump("fsi")
with ex -> ex.Dump("caught")

printfn "finished"