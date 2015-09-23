open System
open System.IO
open System.Diagnostics

//idea for runProc http://stackoverflow.com/questions/15988917/how-can-i-set-the-window-text-of-an-application-using-net-process-start
//open System.Runtime.InteropServices
//
//module private Imported = 
//    [<DllImport("user32.dll")>]
//    extern bool SetWindowText(IntPtr hWnd, string text);

type Result<'a,'b> = |Success of 'a | Failure of 'b

type Path = |Path of string
type Input = |SrcPath of string |Raw of string

let combine s1 s2 = Path.Combine(s1,s2)
let delimit s (items:string seq) = String.Join(s,items)
let copyAllFiles sourceDir targetDir overwrite = 
    Directory.CreateDirectory targetDir |> ignore
    Directory.GetFiles(sourceDir) |> Seq.iter (fun f -> File.Copy(f, combine targetDir (Path.GetFileName f), overwrite))

let runProc filename args startDir = 
    let procStartInfo = 
        ProcessStartInfo(
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            FileName = filename,
            Arguments = args
        )
    match startDir with | Some d -> procStartInfo.WorkingDirectory <- d | _ -> ()

    let outputs = System.Collections.Generic.List<string>()
    let errors = System.Collections.Generic.List<string>()
    let outputHandler f (_sender:obj) (args:DataReceivedEventArgs) = f args.Data
    let p = new Process(StartInfo = procStartInfo)
    p.OutputDataReceived.AddHandler(DataReceivedEventHandler (outputHandler outputs.Add))
    p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (outputHandler errors.Add))
    let started = 
        try
            p.Start()
        with | ex ->
            ex.Data.Add("filename", filename)
            reraise()
    if not started then
        failwithf "Failed to start process %s" filename
    printfn "Started %s with pid %i" p.ProcessName p.Id
    p.BeginOutputReadLine()
    p.BeginErrorReadLine()
    p.WaitForExit()
    let cleanOut l = l |> Seq.filter (fun o -> String.IsNullOrEmpty o |> not)
    cleanOut outputs,cleanOut errors

let slnFolder = 
    let source = __SOURCE_DIRECTORY__
    combine source ".." |> Path.GetFullPath

let dbFolders = 
    ["ApplicationDatabase"] |> Seq.map (combine slnFolder)

let targetFolder = @"C:\VSDBCMD"

//type ConsoleLogger = {Output:string seq; Errors: string seq}
//
//type MsBuildResult = 
//    | Console of ConsoleLogger
//    | Xml of Path

let msbuild targetProject buildArgs = 
    let targetFolder = Path.GetDirectoryName targetProject
    let msbuildPath = @"C:\Program Files (x86)\MSBuild\14.0\Bin\MSBuild.exe"
    let args = targetProject::buildArgs |> delimit " "
    runProc msbuildPath args (Some targetFolder)

//buildDbProj sample usages:
// buildDbProj (dbFolders |> Seq.skip(1) |> Seq.head) (Some @"C:\TFS\XpressCharts\CustomBuildActivities\BCustomBuildTasks.dll");;
//dbFolders |> Seq.skip 1 |> Seq.head |> buildDbProj None
//dbFolders |> Seq.skip 1 |> Seq.head |> buildDbProj (Some @"C:\TFS\XpressCharts\CustomBuildActivities\BCustomBuildTasks.dll")
//dbFolders |> Seq.head |> buildDbProj None;;

let buildDbProj loggerPath targetFolder = 
    let dbProjFile = Directory.GetFiles(targetFolder, "*.dbproj") |> Seq.head
    let logger = 
        match loggerPath with
        | Some lp -> sprintf "/logger:XMLLogger,%s" lp
        | None -> ""
    let args = 
        logger::["/target:Build";]
        |> List.filter (String.IsNullOrWhiteSpace >> not)
    let outputs,errors = msbuild dbProjFile args
    printfn "%A" (Array.ofSeq outputs)
    if Seq.exists (fun _ -> true) errors then
        printfn "errors %A" (Array.ofSeq errors)
        failwithf "Build failed"
    let xml = outputs |> Seq.last
    targetFolder,xml

let findNewest path = 
    Directory.GetFiles path
    |> Seq.map File.GetLastWriteTime
    |> Seq.max

let copyDbProjOutputs dbProjFolder target = //assume all output to a /sql folder
    let sqlFolder = combine dbProjFolder "sql"
    let newest = 
        [combine sqlFolder "debug"; combine sqlFolder "release"]
        |> Seq.filter Directory.Exists
        |> Seq.filter (Directory.GetFiles >> Seq.exists(fun _ -> true))
        |> Seq.maxBy findNewest
    copyAllFiles newest target true

//end environment purity section

// sample uses:
//dbFolders |> Seq.head |> copyForDeploy;;
let copyForDeploy dbProjFolder =
    let deployFolder = @"C:\VSDBCMD\deployment"
    copyDbProjOutputs dbProjFolder deployFolder
    deployFolder

//@"C:\VSDBCMD\deployment\deployXPApplication.cmd"
let runDeploy cmd = 
    let cmdText = File.ReadAllLines(cmd)
    let badLines = cmdText |> Seq.mapi (fun i line -> (i,line),line.TrimStart().StartsWith("pause")) |> Seq.filter snd |> Seq.map fst
    if badLines |> Seq.exists (fun _ -> true) then
        failwithf "bad lines found %A" (Array.ofSeq badLines)
    runProc cmd "nopause" None

let runPmApp() = 
    let folder = dbFolders |> Seq.find (fun c -> c.Contains"Application")
    let deployFolder = folder |> buildDbProj None |> fst |> copyForDeploy
    let output,errors = runDeploy @"C:\VSDBCMD\deployment\deployPmApplication.cmd"
    printfn "output: %A" (Array.ofSeq output)
    printfn "errors %A" (Array.ofSeq errors)

let runPmApp'() = 
    let folder = dbFolders |> Seq.find (fun c -> c.Contains"Application")
    let deployFolder = folder |> buildDbProj None |> fst |> copyForDeploy
    let output,errors = runProc @"C:\VSDBCMD\vsdbcmd.exe" """/a:Deploy /cs:"Server=.;Integrated Security=true" /dsp:Sql /dd+ /manifest:"C:\VSDBCMD\deployment\ApplicationDatabase.deploymanifest" /p:AlwaysCreateNewdatabase=False /p:PerformDatabaseBackup=False /p:IgnoreColumnCollation=True""" (Some @"C:\VSDBCMD\")
    printfn "output: %A" (Array.ofSeq output)
    printfn "errors %A" (Array.ofSeq errors)
