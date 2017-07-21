<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll</Reference>
  <Reference>C:\TFS\PracticeManagement\dev\PracticeManagement\Pm.Web\bin\Pm.Web.exe</Reference>
  <Reference>C:\TFS\PracticeManagement\dev\PracticeManagement\Pm.Web\bin\Suave.dll</Reference>
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
Pm.Web.RicoSuave.run None (Some logger) @"C:\TFS\PracticeManagement\dev\PracticeManagement\Pm.Web" port