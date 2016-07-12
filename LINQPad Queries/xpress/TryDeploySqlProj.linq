<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX64&gt;\Microsoft SQL Server\120\SDK\Assemblies\Microsoft.SqlServer.ConnectionInfo.dll</Reference>
  <Reference>&lt;ProgramFilesX64&gt;\Microsoft SQL Server\120\SDK\Assemblies\Microsoft.SqlServer.Smo.dll</Reference>
  <NuGetReference>Rx-Main</NuGetReference>
</Query>

// build and deploy a .sqlproj -> .dacpac
let dc = new TypedDataContext()

let dumpt (t:string) x = x.Dump(t); x
let after (delimiter:string) (x:string) = x.Substring(x.IndexOf(delimiter) + delimiter.Length)

// getting this into CI http://stackoverflow.com/questions/15556339/how-to-build-sqlproj-projects-on-a-build-server?rq=1

let stringEqualsI s1 (toMatch:string)= toMatch <> null && toMatch.Equals(s1, StringComparison.InvariantCultureIgnoreCase)
let (|StartsWithI|_|) (toMatch:string) (x:string) = 
    if not <| isNull x && not <| isNull toMatch && toMatch.Length > 0 && x.StartsWith(toMatch, StringComparison.InvariantCultureIgnoreCase) then
        Some () 
    else None

let replace (target:string) (replacement) (str:string) = str.Replace(target,replacement)

type Railway<'t> =
    | Success of 't
    | Failure of exn
    
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

    let private setupRunProc filename args startDir fBothOpt outF errorF = 
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
        let outputHandler isError f (_sender:obj) (args:DataReceivedEventArgs) = 
            let result = f args.Data
            match fBothOpt with
            | Some fBoth -> fBoth isError args.Data
            | None -> ()
            
            result
            
        let p = new Process(StartInfo = procStartInfo)
        let filterS f s = if not (String.IsNullOrEmpty s) then f s
        
        p.OutputDataReceived.AddHandler(DataReceivedEventHandler (outputHandler false (filterS outF)))
        p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (outputHandler true (filterS errorF)))

        let started = 
            try
                p.Start()
            with ex ->
                ex.Data.Add("filename", filename)
                reraise()
        if not started then
            failwithf "Failed to start process %s" filename
        printfn "Started %s with pid %i" p.ProcessName p.Id
        let setupKillLinq () = // TODO: try to get the liveKillLink:observable<HyperLinq> deal working
            let killIt () = 
                p.Id.Dump("killing process")
                p.Kill()
            let h = Hyperlinq(Action(killIt),sprintf "Kill Process:%i" p.Id, runOnNewThread = true)
            h.Dump()
            

        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        setupKillLinq()
        let onFinish = 
            async{
//                try
                    try
                        p.WaitForExit()
                        timer.Stop()
                        printfn "Finished %s after %A milliseconds" filename timer.ElapsedMilliseconds
//                        liveKillLink.OnCompleted()
                        return Railway.Success(p,timer)
                    with ex -> 
//                        liveKillLink.OnNext(null)
//                        liveKillLink.OnError(ex)
//                        liveKillLink.OnCompleted()
//                        liveKillLink.Dispose()
                        return Railway.Failure ex
            }
        onFinish

    let runProcAsync filename args startDir fBothOpt fOutput fError=
        let outputs = System.Collections.Generic.List<string>()
        let errors = System.Collections.Generic.List<string>()
        let tree f1 s = 
            f1 s
            s
        let onFinish = setupRunProc filename args startDir fBothOpt (tree fOutput>>outputs.Add) (tree fError >> errors.Add)
        let resultTask = 
            async {
                let! result = onFinish
                match result with
                | Railway.Success t ->
                    let p,timer = t
                    return Railway.Success(outputs,errors)
                | Railway.Failure ex -> 
                    return Railway.Failure ex
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
            runProcAsync filename args startDir None outputHandler errorHandler

        let result = Async.RunSynchronously resultTask
        result

    let runProcSync filename args startDir fBothOpt fOutputOpt fErrorOpt = 
        let outputs = System.Collections.Generic.List<string>()
        let errors = System.Collections.Generic.List<string>()
        let fOutput txt = 
            outputs.Add txt
            match fOutputOpt with
            |Some f -> 
                f txt
            | None ->
                ()
        let fError txt =
            errors.Add txt
            match fErrorOpt with
            |Some f ->
                f txt
            | None -> ()
         
        let r = Async.RunSynchronously (setupRunProc filename args startDir fBothOpt fOutput fError)
        outputs,errors
        
        
module MsBuild = 
    let msbuild targetProject buildArgs fBothOpt fOutputOpt fErrorOpt = 
        let targetFolder = Path.GetDirectoryName targetProject
        let msbuildPath = @"C:\Program Files (x86)\MSBuild\14.0\Bin\MSBuild.exe"
        let errorCount outputs errors = 
            let regex = System.Text.RegularExpressions.Regex(@"^\s*([1-9][0-9]*)\s+Error\(s\)$|Build FAILED.")
            [ outputs;errors] |> Seq.concat  |> Seq.map regex.Match |> Seq.tryFind(fun m -> m.Success)
    
        let args = targetProject::buildArgs |> delimit " "
       
        //liveMessageStream.OnNext args.Data
        //liveMessageStream.OnCompleted()
        
        let output,errors = Process'.runProcSync msbuildPath args (Some targetFolder) fBothOpt fOutputOpt fErrorOpt

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
        let tp,tf = 
            Path.GetTempFileName() 
            |> fun tf -> Path.GetDirectoryName tf, Path.GetFileName tf
        let targetFile = Path.Combine(tp, titling + "."+  tf + ".txt")
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
        let outputs,errors = msbuild sqlProjFile args None None None
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
        |> dumpt "subScripts"
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
        use countValue = new System.Reactive.Subjects.BehaviorSubject<int>(0)
        use runValue = new System.Reactive.Subjects.BehaviorSubject<int>(0)
        use progressValue = new System.Reactive.Subjects.BehaviorSubject<int>(0)
        progressValue.DumpLatest("% progress") |> ignore
        subScripts.Length.Dump("todo") 
        countValue.DumpLatest("subScripts processed") |> ignore
        runValue.DumpLatest("subScripts Completed") |> ignore
        let commandGroups = 
            subScripts 
            |> Seq.map (fun lines -> 
                lines 
                |>Seq.map(fun line ->
                                let items = 
                                    seq{  
                                        match line with
                                        |StartsWithI ":r " -> yield! getSubScripts (line |> after ":r " |> fun p -> Path.Combine(Path.GetDirectoryName path,p)) |> Seq.collect id
                                        | _ -> yield line
                                        }
                                    |> List.ofSeq
                                items
                
                ) 
                |> Seq.collect id)
        commandGroups.Dump("Command Groups")
        commandGroups
        |> Seq.iter(fun lines -> 
            let prog = float count / float subScripts.Length * 100.0  |> int
            Util.Progress <- Nullable prog
            progressValue.OnNext(prog)
            runValue.OnNext(run)
            countValue.OnNext(count)
            // handle or throw on sqlcmd call
            if lines |> Seq.exists(fun l -> l.StartsWith(":")) then
                let mutable ignoredAnyLine = false
                count <- count + 1
                for line in lines do
                    
                    match line with
                    | StartsWithI ":setvar" ->
                        //setVars.[line |> before
                        ()
                    | ":on error exit" -> ()
                    | StartsWithI ":r "
                    | StartsWithI ":" ->
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
    | BuildThenUseSqlPackageExe
    | BuildThenUseSqlPackageExeWithPreCompare
    | UseSqlPackageExeWithPreCompare
    //| RunSmo 
    
let runDeploy dbName deployBehavior = 
    // sql output folder and app output folder are now one and the same
    let projFolder = @"C:\TFS\PracticeManagement\dev\PracticeManagement\Db"
    
    let outFolder = @"C:\TFS\PracticeManagement\dev\PracticeManagement\bin\sql"
    
    outFolder.Dump("dacpac folder")
    let useSqlPackageExe () = 
        
            let cmd,args = 
                let targetDacPac = System.IO.Path.Combine(outFolder, dbName)
                let targetBehavior = TargetBehavior.ConnectionString dc.Connection.ConnectionString
                cmdLine targetDacPac DoNotDropObjectsNotInSource true Environment.MachineName targetBehavior
            (cmd,args).Dump()
            use liveMessageStream = 
                let r = new System.Reactive.Subjects.BehaviorSubject<String>(String.Empty)
                r.DumpLatest() |> ignore
                r
            let sb = StringBuilder()
            let fBoth isError msg = // markdown
                //todo: account for whitespacing issues (marks must tightly contain the things they are wrapping no leading/trailing whitespace)
                if isError then 
                    sb.AppendLine (sprintf "**%s**" msg) 
                else 
                    sb.AppendLine msg
                |> ignore<StringBuilder>
            
            let mutable success = false
            try
                    let _,errors = Process'.runProcSync cmd args None (Some (fun isError msg -> fBoth isError msg; liveMessageStream.OnNext msg)) None None
                    if errors.Any() then
                        displayText ([| sb.ToString() |]) "Markdown"    
                        //Util.OnDemand("NonErrorOutput", fun () -> output).Dump()
                        //errors.Dump("errors found")
                        //displayText (errors |> Array.ofSeq) "Errors"
                    else
                        success <- true
                        liveMessageStream.OnCompleted()
            with ex -> 
                    ex.Message.Dump()
                    displayText [| ex.Message |]
                    liveMessageStream.OnError(ex)
                    reraise()
    let buildBehavior () = 
        
        // clean output dir before build
        outFolder
        |> Directory.GetFiles
        |> Array.iter System.IO.File.Delete
        
        outFolder
        |> Directory.GetDirectories
        |> Array.iter (fun d-> IO.Directory.Delete(d,true))
        
        buildSqlProj None projFolder
        |> Dump
        |> ignore
    match deployBehavior with
    | BuildThenUseSqlPackageExeWithPreCompare ->
        buildBehavior()
        let preCompareOpt = 
            Directory.EnumerateFiles(outFolder, "*.sql", SearchOption.AllDirectories) 
            |> dumpt "precompare files found" 
            |> Seq.tryFind (fun fPath -> fPath.EndsWith("PreCompareScript.sql") )
        match preCompareOpt with
        | Some preCompare ->SqlMgmt.migrate preCompare dbName
        | None -> failwithf "PreCompareScript not found"
        printfn "PreCompare finished"
        useSqlPackageExe ()
    | BuildThenUseSqlPackageExe -> 
        buildBehavior()
        useSqlPackageExe ()
        ()
    | UseSqlPackageExeWithPreCompare ->
        let preCompareOpt = 
            Directory.EnumerateFiles(outFolder, "*.sql", SearchOption.AllDirectories) 
            |> dumpt "precompare files found" 
            |> Seq.tryFind (fun fPath -> fPath.EndsWith("PreCompareScript.sql") )
        match preCompareOpt with
        | Some preCompare ->SqlMgmt.migrate preCompare dbName
        | None -> failwithf "PreCompareScript not found"
        printfn "PreCompare finished"
        useSqlPackageExe ()
//    | RunSmo ->
//        SqlMgmt.migrate @"C:\TFS\PracticeManagement\dev\PracticeManagement\Db\sql\debug\PmMigration.publish.sql" dbName
    
    
//
//let copyDbProjOutputs sqlFolder target = //assume all output to a /sql folder
//    copyAllFiles sqlFolder target true
runDeploy "PmMigration" BuildThenUseSqlPackageExeWithPreCompare

//runDeploy "PmMigration" RunSmo

//Util.Cmd(cmdLine).Dump()