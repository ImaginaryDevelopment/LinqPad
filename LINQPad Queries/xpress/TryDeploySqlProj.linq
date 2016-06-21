<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX64&gt;\Microsoft SQL Server\120\SDK\Assemblies\Microsoft.SqlServer.ConnectionInfo.dll</Reference>
  <Reference>&lt;ProgramFilesX64&gt;\Microsoft SQL Server\120\SDK\Assemblies\Microsoft.SqlServer.Smo.dll</Reference>
  <NuGetReference>Rx-Main</NuGetReference>
</Query>

let dc = new TypedDataContext()

let dumpt (t:string) x = x.Dump(t); x

// getting this into CI http://stackoverflow.com/questions/15556339/how-to-build-sqlproj-projects-on-a-build-server?rq=1

let stringEqualsI s1 (toMatch:string)= toMatch <> null && toMatch.Equals(s1, StringComparison.InvariantCultureIgnoreCase)
    
let (|StartsWithI|_|) s1 (toMatch:string) = if stringEqualsI s1 toMatch then Some() else None

let replace (target:string) (replacement) (str:string) = str.Replace(target,replacement)
module Seq =
  /// Iterates over elements of the input sequence and groups adjacent elements.
  /// A new group is started when the specified predicate holds about the element
  /// of the sequence (and at the beginning of the iteration).
  ///
  /// For example: 
  ///    Seq.groupWhen isOdd [3;3;2;4;1;2] = seq [[3]; [3; 2; 4]; [1; 2]]
  let groupWhen f (input:seq<_>) = seq {
    use en = input.GetEnumerator()
    let running = ref true
    
    // Generate a group starting with the current element. Stops generating
    // when it founds element such that 'f en.Current' is 'true'
    let rec group() = 
      [ yield en.Current
        if en.MoveNext() then
          if not (f en.Current) then yield! group() 
        else running := false ]
    
    if en.MoveNext() then
      // While there are still elements, start a new group
      while running.Value do
        yield group() |> Seq.ofList }

module SqlCmdExe =
    type DropBehavior =
        |DoNotDropObjectsNotInSource
        |DropObjectsNotInSource
        |DropObjectsNotInSourceExcept of string list // /p:DoNotDropObjectTypes=ApplicationRoles;Certificates;Credentials;DatabaseRoles;Filegroups;FileTables;LinkedServerLogins;LinkedServers;Logins;Permissions;ServerAuditSpecifications;ServerRoleMembership;ServerRoles;ServerTriggers;Users
    type TargetBehavior = 
        |ServerNameDbName of string * string
        |ConnectionString of string
        
    let cmdLine srcFile dropBehavior blockOnPossibleDataLoss environmentName targetBehavior = 
        let dropObjectsNotInSource,excludeOpt = 
            match dropBehavior with
            | DoNotDropObjectsNotInSource -> false, None
        let targetInfo = 
            match targetBehavior with 
            | ServerNameDbName (targetServer,targetDbName) ->
                sprintf """/TargetDatabaseName:%s%s/TargetServerName:%s%s""" targetDbName Environment.NewLine targetServer Environment.NewLine
            | ConnectionString cs -> sprintf """%s/tcs:"%s"%s""" Environment.NewLine cs Environment.NewLine
        // sql package.ExecutionContext command line reference: https://msdn.microsoft.com/en-us/hh550080(v=vs.103).aspx
        let cmd = @"""C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\Extensions\Microsoft\SQLDB\DAC\130\sqlpackage.exe"""
        sprintf """ /Action:Publish 
                    /SourceFile:"%s.dacpac" 
                    %s
                    /p:DropObjectsNotInSource=%b 
                    /p:GenerateSmartDefaults=true 
                    /p:BlockOnPossibleDataLoss=%b
                    /p:ExcludeObjectTypes=Certificates;Credentials;DatabaseRoles;Filegroups;FileTables;LinkedServerLogins;LinkedServers;Logins;ServerAuditSpecifications;ServerRoleMembership;ServerRoles;ServerTriggers;Users
                    /Variables:Environment=%s""" srcFile targetInfo dropObjectsNotInSource blockOnPossibleDataLoss environmentName
        |> replace Environment.NewLine String.Empty
        |> fun args -> cmd,args

open System
open System.IO
open System.Diagnostics

//TODO: add get latest to target 

//idea for runProc http://stackoverflow.com/questions/15988917/how-can-i-set-the-window-text-of-an-application-using-net-process-start
//open System.Runtime.InteropServices
//
//module private Imported = 
//    [<DllImport("user32.dll")>]
//    extern bool SetWindowText(IntPtr hWnd, string text);
#if INTERACTIVE
#r "System.IO.Compression"
#r "System.IO.Compression.FileSystem"
#endif
open System.IO.Compression

let combine s1 s2 = Path.Combine(s1,s2)
let combineAll ([<ParamArray>]args) = args |> Array.ofSeq |> Path.Combine
let delimit s (items:string seq) = String.Join(s,items)
let copyAllFiles sourceDir targetDir overwrite = 
    Directory.CreateDirectory targetDir |> ignore
    Directory.GetFiles(sourceDir) |> Seq.iter (fun f -> File.Copy(f, combine targetDir (Path.GetFileName f), overwrite))

module Process' = 
    open System.Collections.ObjectModel
    type RunProcResult = {Outputs:string ObservableCollection; Errors: string ObservableCollection; }

    let private setupRunProc filename args startDir outF errorF = 
        let timer = System.Diagnostics.Stopwatch.StartNew()
        let procStartInfo = 
            ProcessStartInfo(
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                FileName = filename,
                Arguments = args
            )
        match startDir with | Some d -> procStartInfo.WorkingDirectory <- d | _ -> ()

        let liveMessageStream = System.Reactive.Subjects.BehaviorSubject<String>(String.Empty)
        liveMessageStream.DumpLatest() |> ignore
        let outputHandler f (_sender:obj) (args:DataReceivedEventArgs) = 
            liveMessageStream.OnNext args.Data
            f args.Data
        let p = new Process(StartInfo = procStartInfo)
        let filterS f s = if not (String.IsNullOrEmpty s) then f s
        
        p.OutputDataReceived.AddHandler(DataReceivedEventHandler (outputHandler (filterS outF)))
        p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (outputHandler (filterS errorF)))

        let started = 
            try
                p.Start()
            with | ex ->
                ex.Data.Add("filename", filename)
                reraise()
        if not started then
            failwithf "Failed to start process %s" filename
        printfn "Started %s with pid %i" p.ProcessName p.Id
        let killIt () = 
            p.Id.Dump("killing process")
            p.Kill()
        let liveKillLink = System.Reactive.Subjects.BehaviorSubject<Hyperlinq>(Hyperlinq(killIt,"Kill Process"))
        liveKillLink.DumpLatest()
        
        
        
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        let onFinish = 
            async{
                try
                    p.WaitForExit()
                    timer.Stop()
                    printfn "Finished %s after %A milliseconds" filename timer.ElapsedMilliseconds
                    liveMessageStream.OnCompleted()
                    liveKillLink.OnCompleted()
                with ex -> 
                    liveKillLink.OnCompleted()
                    liveMessageStream.OnCompleted()
                    reraise()
                return (p,timer)
            }
        onFinish

    let runProcAsync filename args startDir fOutput fError=
        let outputs = System.Collections.Generic.List<string>()
        let errors = System.Collections.Generic.List<string>()
        let tree f1 s = 
            f1 s
            s
        let onFinish = setupRunProc filename args startDir (tree fOutput>>outputs.Add) (tree fError >> errors.Add)
        let resultTask = 
            async {
                let! p,timer = onFinish
                return outputs,errors
            }
        resultTask

    let runProcPrint filename args startDir outputIsErrorF = 
        let errorHandler errS= 
            let color = System.Console.ForegroundColor
            Console.ForegroundColor <- ConsoleColor.Red
            Console.WriteLine (sprintf "err:%s" errS)
            Console.ForegroundColor <- color

        let outputHandler s = 
            match outputIsErrorF with
            |Some f when f s -> errorHandler s
            | _ -> printfn "%s" s

        let resultTask = 
            async {
                let! output,errors= runProcAsync filename args startDir outputHandler errorHandler
                return (output,errors)
            }

        let outputs,errors = Async.RunSynchronously resultTask
        outputs,errors

    let runProcSync filename args startDir = 
        let outputs = System.Collections.Generic.List<string>()
        let errors = System.Collections.Generic.List<string>()
        let p,timer = Async.RunSynchronously (setupRunProc filename args startDir outputs.Add errors.Add)
        outputs,errors
        
        
module MsBuild = 
    let msbuild targetProject buildArgs = 
        let targetFolder = Path.GetDirectoryName targetProject
        let msbuildPath = @"C:\Program Files (x86)\MSBuild\14.0\Bin\MSBuild.exe"
        let errorCount outputs errors = 
            let regex = System.Text.RegularExpressions.Regex(@"^\s*([1-9][0-9]*)\s+Error\(s\)$|Build FAILED.")
            [ outputs;errors] |> Seq.concat  |> Seq.map regex.Match |> Seq.tryFind(fun m -> m.Success)
    
        let args = targetProject::buildArgs |> delimit " "
        let output,errors = Process'.runProcSync msbuildPath args (Some targetFolder)
        match errorCount output errors with
        | Some errorMatch -> 
            let regex = System.Text.RegularExpressions.Regex("Build error", Text.RegularExpressions.RegexOptions.IgnoreCase)
    
            printfn "%A" (output |> Seq.filter regex.IsMatch |> List.ofSeq)
            let errorText = 
                let text = errorMatch.Groups.[1].Value
                if String.IsNullOrWhiteSpace(text) then errorMatch.Groups.[0].Value else text
            failwithf "ErrorsFound : %s" errorText
        | None -> ()
        if output |> Seq.contains ("Build FAILED.") then failwithf "Build failed"
        output,errors
    
    let displayText lines titling = 
        let targetFile = Path.Combine(Path.GetTempPath(), titling, Path.GetTempFileName() + ".txt")
        File.WriteAllLines(targetFile, lines)
        let p = Process.Start(targetFile)
        printfn "Started textfile %s with id %i @ %s" titling p.Id targetFile
    
    let buildSqlProj loggerPath targetFolder = 
        let sqlProjFile = Directory.GetFiles(targetFolder, "*.sqlproj") |> Seq.head
        let logger = 
            match loggerPath with
            | Some lp -> sprintf "/logger:XMLLogger,%s" lp
            | None -> ""
        let args = 
            logger::["/target:Build"]
            |> List.filter (String.IsNullOrWhiteSpace >> not)
        printfn "%A" args
        let outputs,errors = msbuild sqlProjFile args
        let outputs = Array.ofSeq outputs
        displayText outputs "MsBuild"
        printfn "%A" outputs
        //printfn "error length: %i" (Seq.length errors)
        if Seq.exists (fun _ -> true) errors then
            printfn "errors %A" (Array.ofSeq errors)
            failwithf "Build failed %A" errors
        let xml = outputs |> Seq.last
        targetFolder,xml
    

let findNewest path = 
    Directory.GetFiles path
    |> Seq.map File.GetLastWriteTime
    |> Seq.max

let getNewestProjFolder projFolder = //debug or release whichever is newer 
    let sqlFolder = combine projFolder "sql"
    [combine sqlFolder "debug"; combine sqlFolder "release"]
    |> Seq.filter Directory.Exists
    |> Seq.filter (Directory.GetFiles >> Seq.exists(fun _ -> true))
    |> Seq.maxBy findNewest


module SqlMgmt = 
    let getSubScripts path = 
        seq{
            let text = File.ReadLines(path) |> Array.ofSeq // @"C:\TFS\PracticeManagement\dev\PracticeManagement\Db\sql\debug\PmMigration.publish.sql"
            yield! text|> Seq.groupWhen(stringEqualsI "GO")|> Seq.map (Seq.filter (fun l -> not <| stringEqualsI "GO" l && not <| String.IsNullOrWhiteSpace l))
        }
        |> List.ofSeq

    let migrate path dbName =
        use cn = new SqlConnection(dc.Connection.ConnectionString)
        let sc = Microsoft.SqlServer.Management.Common.ServerConnection(cn)
        let server = new Microsoft.SqlServer.Management.Smo.Server(sc)
        let setVars = Dictionary<string,string>()
        setVars.["DatabaseName"] <- dbName
        let mutable lastRun = String.Empty
        let mutable run,count = 0,0
        let subScripts = getSubScripts path
        use countValue = System.Reactive.Subjects.BehaviorSubject<int>(0)
        use runValue = System.Reactive.Subjects.BehaviorSubject<int>(0)
        use progressValue = System.Reactive.Subjects.BehaviorSubject<int>(0)
        progressValue.DumpLatest("% progress")
        subScripts.Length.Dump("todo")
        countValue.DumpLatest("subScripts processed")
        runValue.DumpLatest("subScripts Completed")
        
        
        subScripts
        |> Seq.iter(fun lines -> 
            let prog = float count / float subScripts.Length * 100.0  |> int
            Util.Progress <- Nullable prog
            progressValue.OnNext(prog)
            runValue.OnNext(run)
            countValue.OnNext(count)
            if lines |> Seq.exists(fun l -> l.StartsWith(":")) then
                let mutable ignoredAnyLine = false
                count <- count + 1
                for line in lines do
                    
                    match line with
                    | StartsWithI ":setvar" ->
                        //setVars.[line |> before
                        ()
                    | ":on error exit" -> ()
                    | ":" ->
                        line.Dump("failed to match sqlcmd")
                        raise <| ArgumentOutOfRangeException(line)
                        ()
                    | _ -> 
                        ignoredAnyLine <- true
                if ignoredAnyLine then lines.Dump("has ignored line(s)")
            else
                count <- count + 1
                let text = String.Join(Environment.NewLine, lines)
                let reKeyed = text |> fun line -> setVars.Keys |> Seq.fold (fun line replaceKey -> line |> replace (sprintf "$(%s)" replaceKey) setVars.[replaceKey]) line
                try
                    reKeyed
                    |> server.ConnectionContext.ExecuteNonQuery
                    |> ignore<int>
                    lastRun <- reKeyed
                    run <- run + 1
                with ex -> 
                    lastRun.Dump("last completed operation")
                    (lines,setVars,ex).Dump()
                    reraise()
            )
        countValue.OnCompleted()
        runValue.OnCompleted()
        progressValue.OnCompleted()
        
        lastRun.Dump("last completed operation")
        (run,count).Dump("run, counted sub-scripts")
        
open MsBuild
open SqlMgmt
open SqlCmdExe

type DeployBehavior = 
    | UseSqlPackageExe
    | UseSqlPackageExeWithPreCompare
    | RunSmo 
    
let runDeploy dbName deployBehavior = 
    let projFolder = @"C:\TFS\PracticeManagement\dev\PracticeManagement\Db"
    // clean output dir before build
    Path.Combine(projFolder,"sql", "debug")
    |> Directory.GetFiles
    |> Array.iter System.IO.File.Delete
    
    buildSqlProj None projFolder
    |> Dump
    |> ignore

    let newestChild = getNewestProjFolder projFolder

    newestChild.Dump("dacpac folder")
    let useSqlPackageExe () = 
        
            let cmd,args = 
                let targetDacPac = System.IO.Path.Combine(newestChild, dbName)
                let targetBehavior = TargetBehavior.ConnectionString dc.Connection.ConnectionString
                cmdLine targetDacPac DoNotDropObjectsNotInSource true Environment.MachineName targetBehavior

            (cmd,args).Dump()
            try
                //  filename args startDir
                let output,errors = Process'.runProcSync cmd args None
                (output,errors).Dump()
                //Util.Cmd(fullCmdLine,false) |> ignore<string[]>
            with ex -> 
                ex.Message.Dump()
                displayText [| ex.Message |]
                reraise()
        
    match deployBehavior with
    | UseSqlPackageExeWithPreCompare ->
        let preCompareOpt = 
            Directory.EnumerateFiles(newestChild, "*.sql", SearchOption.AllDirectories) 
            |> dumpt "files found" 
            |> Seq.tryFind (fun fPath -> fPath.EndsWith("PreCompareScript.sql") )
        match preCompareOpt with
        | Some preCompare ->SqlMgmt.migrate preCompare dbName
        | None -> failwithf "PreCompareScript not found"
        printfn "PreCompare finished"
        useSqlPackageExe ()
    | UseSqlPackageExe -> 
        useSqlPackageExe ()
        ()
    | RunSmo ->
        SqlMgmt.migrate @"C:\TFS\PracticeManagement\dev\PracticeManagement\Db\sql\debug\PmMigration.publish.sql" dbName
    
    
//
//let copyDbProjOutputs sqlFolder target = //assume all output to a /sql folder
//    copyAllFiles sqlFolder target true
runDeploy "PmMigration" UseSqlPackageExeWithPreCompare

//runDeploy "PmMigration" RunSmo

//Util.Cmd(cmdLine).Dump()