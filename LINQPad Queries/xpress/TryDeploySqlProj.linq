<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX64&gt;\Microsoft SQL Server\120\SDK\Assemblies\Microsoft.SqlServer.ConnectionInfo.dll</Reference>
  <Reference>&lt;ProgramFilesX64&gt;\Microsoft SQL Server\120\SDK\Assemblies\Microsoft.SqlServer.Smo.dll</Reference>
  <NuGetReference>Rx-Main</NuGetReference>
</Query>

// build and deploy a .sqlproj -> .dacpac
let dc = new TypedDataContext()
type System.String with
    static member subString i (x:string) = x.Substring(i)
    static member subString2 i e (x:string)= x.Substring(i,e)
    static member contains s (x:string) = x.Contains(s)
    
let dumpt (t:string) x = x.Dump(t); x
let after (delimiter:string) (s:string) = s|> String.subString (s.IndexOf delimiter + delimiter.Length)
let before (delimiter:string) s = s|> String.subString2 0 (s.IndexOf delimiter)
let afterOrSelf delimiter x = if x|> String.contains delimiter then x |> after delimiter else x
let beforeOrSelf delimiter x = if x|> String.contains delimiter then x |> before delimiter else x
let replace (target:string) (replacement) (str:string) = str.Replace(target,replacement)
let teeTuple f x = x, f x

// getting this into CI http://stackoverflow.com/questions/15556339/how-to-build-sqlproj-projects-on-a-build-server?rq=1

let stringEqualsI s1 (toMatch:string)= toMatch <> null && toMatch.Equals(s1, StringComparison.InvariantCultureIgnoreCase)
let (|StartsWithI|_|) (toMatch:string) (x:string) = 
    if not <| isNull x && not <| isNull toMatch && toMatch.Length > 0 && x.StartsWith(toMatch, StringComparison.InvariantCultureIgnoreCase) then
        Some () 
    else None

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
        
        
module SqlHelpers = 
    type NonBracketedName = NonBracketedName of string 
        with override x.ToString() = match x with | NonBracketedName.NonBracketedName s -> s
//shadow the constructor
    let NonBracketedName input = 
        if input |> String.contains "[" || input |> String.contains "]" then None
        else Some <| NonBracketedName(input)

    type SprocReference = {Name:NonBracketedName;Schema:NonBracketedName} with
        override x.ToString() = sprintf "[%O].[%O]" (x.Schema |> string) (x.Name |> string)
    type private SprocDefHolder() =
        member val Definition = String.Empty with get,set
        member val ObjectId = 0L with get,set
    
    type SprocManager = {R:SprocReference; Add: string; Drop: string; ObjectId:System.Int64} // don't trust that it is int
    let getManager (sr:SprocReference) drop = 
        let schema,name = sr.Schema |> string, sr.Name |> string
        let text = sprintf @"select 
            --SPECIFIC_CATALOG,SPECIFIC_SCHEMA,SPECIFIC_NAME, 
    case 
        when len(ROUTINE_DEFINITION) > 3999 then 
            (select object_definition([object_id]) 
            from sys.procedures 
            where object_schema_name([object_id]) = '%s' and name='%s') 
        else ROUTINE_DEFINITION end as definition, so.id
from INFORMATION_SCHEMA.routines r
    join dbo.sysobjects so 
    on so.id=object_id(N'[%s].[%s]') and OBJECTPROPERTY(id, N'IsProcedure') = 1
where ROUTINE_TYPE = 'PROCEDURE' AND SPECIFIC_SCHEMA = '%s' and specific_name = '%s'"
                    schema name schema name schema name
        dc.ExecuteQuery<SprocDefHolder>(text)
        |> List.ofSeq
        |> function
            | [x] -> {R=sr; Add=x.Definition; ObjectId=x.ObjectId; Drop=drop}
            | [] -> failwithf "No definition found for %s using %s" name text
            | x -> failwithf "More than one result in seq %A" x
open SqlHelpers
SqlHelpers.getManager {Schema=(NonBracketedName "dbo").Value; Name=(NonBracketedName "uspClaimsInsUpd").Value;} "drop me"
|> Dump

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
        //|> dumpt "subScripts"
        |> List.ofSeq

    let migrate path dbName =
        use cn = new SqlConnection(dc.Connection.ConnectionString)
        let sc = Microsoft.SqlServer.Management.Common.ServerConnection(cn)
        let server = new Microsoft.SqlServer.Management.Smo.Server(sc)
        let setVars = Dictionary<string,string>()
        setVars.["DatabaseName"] <- dbName
        let mutable lastRun = String.Empty
        //let mutable run,count = 0,0
        let subScripts = getSubScripts path
        use countValue = new System.Reactive.Subjects.BehaviorSubject<int>(0)
        use runValue = new System.Reactive.Subjects.BehaviorSubject<int>(0)
        // progress stops at 66%, needs attention/maint
        //use progressValue = new System.Reactive.Subjects.BehaviorSubject<int>(0)
        //progressValue.DumpLatest("% progress") |> ignore
         
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
            |> List.ofSeq
        commandGroups.Length.Dump("todo")
        //commandGroups.Dump("Command Groups")
        commandGroups
        |> Seq.iter(fun lines -> 
            let count = countValue.Value + 1
            let prog = float count / float subScripts.Length * 100.0  |> int
            Util.Progress <- Nullable prog
            //progressValue.OnNext(prog)
            
            countValue.OnNext count
            // handle or throw on sqlcmd call
            if lines |> Seq.exists(fun l -> l.StartsWith(":")) then
                let mutable ignoredAnyLine = false
                //count <- count + 1
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
                //count <- count + 1
                let text = String.Join(Environment.NewLine, lines)
                let reKeyed = text |> fun line -> setVars.Keys |> Seq.fold (fun line replaceKey -> line |> replace (sprintf "$(%s)" replaceKey) setVars.[replaceKey]) line
                try
                    reKeyed
                    |> server.ConnectionContext.ExecuteNonQuery
                    |> ignore<int>
                    lastRun <- reKeyed
                    //run <- run + 1
                    runValue.OnNext(runValue.Value + 1)
                with ex -> 
                    lastRun.Dump("last completed operation")
                    (lines,setVars,ex).Dump()
                    reraise()
            )
        countValue.OnCompleted()
        runValue.OnCompleted()
        //progressValue.OnCompleted()
        
        lastRun.Dump("last completed operation")
        //(run,count).Dump("run, counted sub-scripts")
        
open MsBuild
open SqlMgmt
open SqlCmdExe

type DeployBehavior = 
    | BuildThenUseSqlPackageExe
    | BuildThenUseSqlPackageExeWithPreCompare
    | UseSqlPackageExeWithPreCompare
    //| RunSmo 



//let sprocRefTest = {Name = NonBracketedName("sp_helptext").Value; Schema=NonBracketedName("dbo").Value}
//(sprocRefTest,sprocRefTest.ToString()).Dump()

let runDeploy dbName deployBehavior = 
    // sql output folder and app output folder are now one and the same
    let projFolder = @"C:\TFS\PracticeManagement\dev\PracticeManagement\Db"
    
    let outFolder = @"C:\TFS\PracticeManagement\dev\PracticeManagement\bin\sql"
    
    outFolder.Dump("dacpac folder")
    let withSprocOnlyErrors fDeploy (sprocErrors:#seq<SprocReference*string>) :unit = 
        let getDropSprocText (r:SprocReference) = 
            if r.Schema |> string |> String.contains "[" || r.Name |> string |> String.contains "]" then raise <| invalidOp String.Empty
            let sprocFullName = r |> string
            
            sprintf "IF EXISTS (select * from dbo.sysobjects where id = object_id(N'%s') and OBJECTPROPERTY(id, N'IsProcedure') = 1) \r\n  DROP PROCEDURE %s" sprocFullName sprocFullName
            
        try
            // drop sprocs
            let dropResults = 
//                let sprocErrors =
                    sprocErrors
                    |> Seq.map(fun (r,def) -> {R=r; Add=def; Drop=getDropSprocText r})
//                let sprocErrors =
//                    sprocErrors
                    |> Seq.map (teeTuple (fun m -> m.Drop |> dc.ExecuteCommand))
//                sprocErrors
//                |> List.ofSeq()
            dropResults.Dump()
            // re-call deploy
            fDeploy()
        finally
            // re-add sprocs
            let reAddsWithResults = 
                sprocErrors
                |> Seq.map(teeTuple (snd >> dc.ExecuteCommand))
                |> List.ofSeq
            reAddsWithResults
            |> Seq.iter (function 
                | _,1 -> () 
                |((n,_),x) -> 
                    n.Dump("Failed")
                    reAddsWithResults.Dump("full payload of sproc-readds")
                    failwithf "Add should have returned 1, but was %i instead" x
                )
            //|> Seq.iter(fun (dc.ExecuteCommand
        sprocErrors
        |> dumpt "sprocs to remove"
        |> ignore
        ()
        
    //retry all but the onJustSprocErrorsOpt    
    let useSqlPackageExe fOnJustSprocErrorsOpt  = 
        let cmd,args = 
            let targetDacPac = System.IO.Path.Combine(outFolder, dbName)
            let targetBehavior = TargetBehavior.ConnectionString dc.Connection.ConnectionString
            cmdLine targetDacPac DoNotDropObjectsNotInSource true Environment.MachineName targetBehavior
        (cmd,args).Dump()
        use liveMessageStream = 
            let r = new System.Reactive.Subjects.BehaviorSubject<String>(String.Empty)
            r.DumpLatest() |> ignore
            r
        let allOutsAsMarkdown = StringBuilder()
        let append msg (sb:StringBuilder) = sb.AppendLine msg |> ignore
        let bonafideErrors = List<string>()
        let fBoth isError msg = // markdown
            if not <| String.IsNullOrWhiteSpace msg then
                if isError then 
                    let leadingStarsAndSpace = @"^[* \t]*"
                    let content = @"(\w.*)"
                    let markdowned = Regex.Replace(msg,sprintf "(%s)(%s)[ \t]*" leadingStarsAndSpace content,@"$1**$2**",RegexOptions.Multiline)
                    if msg.StartsWith "Error SQL" then
                        bonafideErrors.Add msg
                    allOutsAsMarkdown |> append markdowned 
                else 
                    allOutsAsMarkdown |> append msg
        
        let mutable success = false
        try
                let _,errors = Process'.runProcSync cmd args None (Some (fun isError msg -> fBoth isError msg; liveMessageStream.OnNext msg)) None None
                if errors.Any() then
                    Util.OnDemand("Full output markdown", fun() -> displayText ([| allOutsAsMarkdown.ToString() |]) "Markdown").Dump()
                    let blocked72031Regex = "Error SQL72031: This deployment may encounter errors during execution because changes to (.*) are blocked by (.*)'s dependency in the target database."
                    let tryGetSprocInfo input = 
                        match Regex.Match(input, blocked72031Regex) with
                        | reg when reg.Success -> 
                            // warning: this table only contains the first 4000 chars, if it is longer it is truncated
                            let routine = dc.INFORMATION_SCHEMA.ROUTINES.FirstOrDefault(fun r -> r.ROUTINE_TYPE = "Procedure" && not (r.ROUTINE_NAME.StartsWith("sp_")))
                            if not <| isNull routine then
                                
                                Some (reg.Groups.[2].Value, routine)
                            else None
                        | _ -> None
                    let getSprocRef fullname =
                        let schema, sprocName = fullname |> afterOrSelf "[" |> before "." |> beforeOrSelf "]", fullname |> after "." |> beforeOrSelf "]" |> afterOrSelf "["
                        { Schema = (NonBracketedName schema).Value; Name= (NonBracketedName sprocName).Value}
                    
                    let sprocErrors = bonafideErrors |> Seq.map tryGetSprocInfo |> List.ofSeq
                    if sprocErrors |> Seq.forall (Option.isSome) then
                        match fOnJustSprocErrorsOpt with
                        | Some f ->
                            // if all errors are of the sproc type, copy sproc definitions, drop them, retry deploy, and re-add them
                            let mapSprocSize (sprocRef:SprocReference,def:string) = 
                                if def.Length >= 3999 then // http://sqlblog.com/blogs/aaron_bertrand/archive/2011/11/08/t-sql-tuesday-24-dude-where-s-the-rest-of-my-procedure.aspx
                                    sprocRef.ToString().Dump("big un") //  untested branch, has not been hit thus far
                                    let query = 
                                        sprintf "select object_definition([object_id]) from sys.procedures where object_schema_name([object_id]) = '%O' and name='%O'" 
                                            sprocRef.Schema sprocRef.Name
                                    sprocRef, (dc.ExecuteQuery<string>(query) |> Seq.head)
                                else sprocRef, def
                                
                            let sprocErrors = 
                                sprocErrors 
                                |> Seq.choose id
                                |> Seq.map(fun (name,routine) -> getSprocRef name, routine.ROUTINE_DEFINITION)
                                |> Seq.map mapSprocSize
                            f sprocErrors 
                        | None -> failwithf "No options for dealing with sproc only errors %A" sprocErrors               
                        ()
                    else
                        bonafideErrors.Dump()
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
                
    // avoiding any possibility of infinite recursion
    
    let useSqlPackageWithSprocRetry ()= useSqlPackageExe (Some(withSprocOnlyErrors (fun () -> useSqlPackageExe None)))
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
        useSqlPackageWithSprocRetry ()
    | BuildThenUseSqlPackageExe -> 
        buildBehavior()
        useSqlPackageWithSprocRetry ()
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
        useSqlPackageWithSprocRetry ()
//    | RunSmo ->
//        SqlMgmt.migrate @"C:\TFS\PracticeManagement\dev\PracticeManagement\Db\sql\debug\PmMigration.publish.sql" dbName

//runDeploy "PmMigration" BuildThenUseSqlPackageExeWithPreCompare
runDeploy "PmMigration" UseSqlPackageExeWithPreCompare

//runDeploy "PmMigration" RunSmo

//Util.Cmd(cmdLine).Dump()