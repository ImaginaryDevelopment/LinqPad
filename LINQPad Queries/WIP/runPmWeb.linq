<Query Kind="FSharpProgram">
  <Reference>C:\tfs\practicemanagement\trunk\Pm.Web\bin\Pm.Dal.dll</Reference>
  <Reference>C:\tfs\practicemanagement\trunk\Pm.Web\bin\Pm.Web.exe</Reference>
  <Reference>C:\tfs\practicemanagement\trunk\Pm.Web\bin\Suave.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
</Query>

let asm = System.Reflection.Assembly.GetExecutingAssembly()
asm.Location |> FileInfo |> fun x -> (asm.Location,x.LastWriteTime) |> Dump
let loaded = Util.OnDemand("assemblies", fun () -> AppDomain.CurrentDomain.GetAssemblies()) |> Dump
//Directory.GetCurrentDirectory() |> Dump
System.Diagnostics.Process.GetCurrentProcess().Id |> Dump |> ignore

//Suave.Web.defaultConfig.Dump()
//let setContent =
//    let dc = DumpContainer(Content=List.empty)
//    Hyperlinq((fun ()-> dc.Content <- List.empty), "Clear").Dump()
//    dc.Dump()
//    
//    fun x -> dc.Content <- x::(dc.Content :?> obj list)
    
let logger ll fLogLine = 
    let ignorePaths = ["Suave.Socket.BufferManager"]
    let filter (line:Suave.Logging.LogLine) = 
        if String.IsNullOrEmpty line.path || ignorePaths |> Seq.contains line.path |> not then
            line
            |> Dump
            |> ignore
    match ll with
    | Suave.Logging.LogLevel.Error
    | Suave.Logging.LogLevel.Warn
    | Suave.Logging.LogLevel.Fatal ->
        let line:Suave.Logging.LogLine = fLogLine()
        line |> filter
    | _ -> ()// fLogLine() |> filter

let port = 8081us
Hyperlinq(sprintf "http://localhost:%i/" port).Dump()
let homeFolder = Path.Combine(Environment.ExpandEnvironmentVariables("%devroot%"),"Pm.Web")
if not <| Directory.Exists homeFolder then
    failwithf "Pm.Web path not found for home folder at %s" homeFolder
let csProvider = Pm.Web.ConnectionStringProvider.Cs Pm.Dal.BuildTime.CString
Pm.Web.RicoSuave.run (Some csProvider) None (Some logger) homeFolder port