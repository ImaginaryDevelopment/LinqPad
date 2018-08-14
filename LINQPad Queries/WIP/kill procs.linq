<Query Kind="FSharpExpression" />

// kill all msbuild and vs related processes
let killList = Set ["msbuild";"VBCSCompiler"; "XDesProc"]
let getDetailsList = Set["devenv"]
let procs = 
    Process.GetProcesses()
    |> Seq.map(fun p -> p.ProcessName,p)
    |> Seq.sortBy (fun (n,_) -> killList |> Seq.contains n |> not, n)
    |> Seq.countBy fst
    
    |> List.ofSeq
(procs).Dump("procs")
//procs
//|> Seq.choose (fun (n,p) -> 
//    if killList |> Seq.contains(n) then
        