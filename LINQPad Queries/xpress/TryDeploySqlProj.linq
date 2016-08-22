<Query Kind="FSharpProgram">
  <Reference Relative="..\..\..\..\FsInteractive\BReusable.dll">C:\projects\FsInteractive\BReusable.dll</Reference>
  <Reference>&lt;ProgramFilesX64&gt;\Microsoft SQL Server\120\SDK\Assemblies\Microsoft.SqlServer.ConnectionInfo.dll</Reference>
  <Reference>&lt;ProgramFilesX64&gt;\Microsoft SQL Server\120\SDK\Assemblies\Microsoft.SqlServer.Smo.dll</Reference>
  <Reference Relative="..\..\..\..\FsInteractive\ProcessMacros.dll">C:\projects\FsInteractive\ProcessMacros.dll</Reference>
  <Reference Relative="..\..\..\..\FsInteractive\SqlMacros.dll">C:\projects\FsInteractive\SqlMacros.dll</Reference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>BReusable</Namespace>
  <Namespace>Macros.ProcessMacros</Namespace>
  <Namespace>Macros.SqlMacros</Namespace>
</Query>

// build and deploy a .sqlproj -> .dacpac
// getting this into CI http://stackoverflow.com/questions/15556339/how-to-build-sqlproj-projects-on-a-build-server?rq=1

let dumpt (t:string) x = x.Dump(t); x

type TypedDataContext with
    member x.ExecuteCommandWithCatch text = 
        try
            x.ExecuteCommand text
        with ex -> 
            ex.Data.Add("datasource", x.Connection.DataSource)
            ex.Data.Add("Database", x.Connection.Database)
            ex.Data.Add("sql",text)
            ex.Data.Add("user_name",x.ExecuteQuery<string>("select user_name()") |> Seq.head)
            ex.Data.Add("suser_name", x.ExecuteQuery<string>("select suser_name()") |> Seq.head)
            reraise()

let dc = new TypedDataContext() 

let cs = dc.Connection.ConnectionString
//cs.Dump()
module SqlHelpers = 
    let tryGetMeta sr t= 
        tryGetMeta (fun s -> dc.ExecuteQuery<ObjectDefHolder> s) sr t
        
module SqlCmdExe =
    open Railways
    open SqlHelpers
    type DropBehavior =
        |DoNotDropObjectsNotInSource
        |DropObjectsNotInSource
        |DropObjectsNotInSourceExcept of string list // /p:DoNotDropObjectTypes=ApplicationRoles;Certificates;Credentials;DatabaseRoles;Filegroups;FileTables;LinkedServerLogins;LinkedServers;Logins;Permissions;ServerAuditSpecifications;ServerRoleMembership;ServerRoles;ServerTriggers;Users
    type TargetBehavior = 
        |ServerNameDbName of string * string
        |ConnectionString of string
        
    type Handleable =
        |Sproc
        |Trigger //of targetTable:string
    
    type SqlPackageError =
        |CanHandle of ObjectReference * Handleable
        |Other of string
//    type Manager = Handleable * ObjectManager
    //let x = Microsoft.SqlServer.Management.Common.ServerConnection

    let getManagerRail (r:ObjectReference) handleable :Railway<ObjectManager,_> = 
        let schema,name = r.Schema |> string, r.Name |> string
        match handleable with
        | Sproc -> tryGetMeta r "P" |> function | Success od -> Success {R = r; Drop=getDropSprocText r; ObjectId=od.Object_Id; Add = od.Definition} | Failure x -> Failure x
        | Trigger -> 
            let t = "TR"
            let getIfExistsDisabler (od:ObjectDefHolder) = getDropTriggerWithExists r
            match tryGetMeta r t with
            | Success od -> Success {R=r; Drop= getIfExistsDisabler od; ObjectId=od.Object_Id; Add= od.Definition}
            | Failure x -> Failure x
            
    let getIsHandleable (r:ObjectReference) : Railway<ObjectReference*Handleable,string list>=
        let attemptResults =
            let tryGetIs (handleable:Handleable) getExistsText : Handleable option =
                try
                    let result = dc.ExecuteQuery<int>(getExistsText) |> List.ofSeq
                    if result |> Seq.head = 1 then 
                        Some handleable 
                    else None
                with ex -> 
                    ex.Data.Add("sql",getExistsText)
                    reraise()
            let fAttempts =
                [   //let teeTuple f x = x, f x
                    Macros.SqlMacros.getIsSprocText >> teeTuple (tryGetIs Handleable.Sproc)
                    Macros.SqlMacros.getIsTriggerText >> teeTuple (tryGetIs Handleable.Trigger)
                ]
            let fAttemptResults = fAttempts |> Seq.map (fun f -> f r)
            fAttemptResults
            |> Seq.map (fun (text:string,hOpt:Handleable option)  ->
                    match hOpt with 
                    | Some h -> Success h 
                    | None -> Failure [sprintf "Could not locate a handler for %O" r; text])
            |> List.ofSeq
        match attemptResults |> Seq.tryFind isSuccess with
        | Some (Success h) -> Success (r,h)
        | None -> 
            let failures = attemptResults |> Seq.choose Railways.toFailureOption
            failures |> Seq.collect id |> List.ofSeq |> Failure

    let cmdLine srcFile dropBehavior blockOnPossibleDataLoss environmentName targetBehavior includeTransactionalScripts = 
        let dropObjectsNotInSource,excludeOpt = 
            match dropBehavior with
            | DoNotDropObjectsNotInSource -> false, None
        let targetInfo = 
            match targetBehavior with 
            | ServerNameDbName (targetServer,targetDbName) ->
                sprintf """/TargetDatabaseName:%s%s/TargetServerName:%s%s""" targetDbName Environment.NewLine targetServer Environment.NewLine
            | ConnectionString cs -> sprintf """%s/tcs:"%s"%s""" Environment.NewLine cs Environment.NewLine
        let otherOptionText = match includeTransactionalScripts with | true -> sprintf "/p:IncludeTransactionalScripts=True" | false -> String.Empty
        
        // sql package.ExecutionContext command line reference: https://msdn.microsoft.com/en-us/hh550080(v=vs.103).aspx
        let cmd = @"""C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\Extensions\Microsoft\SQLDB\DAC\130\sqlpackage.exe"""
        sprintf """ /Action:Publish 
                    /SourceFile:"%s.dacpac" 
                    %s
                    /p:DropObjectsNotInSource=%b 
                    /p:GenerateSmartDefaults=true 
                    /p:BlockOnPossibleDataLoss=%b
                    /p:ExcludeObjectTypes=Certificates;Credentials;DatabaseRoles;Filegroups;FileTables;LinkedServerLogins;LinkedServers;Logins;ServerAuditSpecifications;ServerRoleMembership;ServerRoles;ServerTriggers;Users
                    %s
                    /Variables:Environment=%s""" srcFile targetInfo dropObjectsNotInSource blockOnPossibleDataLoss otherOptionText environmentName
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

let getNewestProjFolder projFolder = //debug or release whichever is newer 
    let sqlFolder = combine projFolder "sql"
    [combine sqlFolder "debug"; combine sqlFolder "release"]
    |> Seq.filter Directory.Exists
    |> Seq.filter (Directory.GetFiles >> Seq.exists(fun _ -> true))
    |> Seq.maxBy BReusable.PathHelpers.findNewest

module SqlMgmt = 
    type Microsoft.SqlServer.Management.Common.ServerConnection with
        member x.ExecuteNonQueryWithCatch (text:string) = 
            try
                x.ExecuteNonQuery(text)
            with ex -> 
                ex.Data.Add("ConnectionString", x.ConnectionString)
                ex.Data.Add("DatabaseName", x.DatabaseName)
                ex.Data.Add("sql", text)
                //x.ConnectionString
                reraise()
    let getSubScripts path = 
        seq{
            let text = File.ReadLines(path) |> Array.ofSeq // @"C:\TFS\PracticeManagement\dev\PracticeManagement\Db\sql\debug\PmMigration.publish.sql"
            yield! text|> Seq.groupWhen(stringEqualsI "GO")|> Seq.map (Seq.filter (fun l -> not <| stringEqualsI "GO" l && not <| String.IsNullOrWhiteSpace l))
        }
        //|> dumpt "subScripts"
        |> List.ofSeq

    let migrate path dbName =
        dc.Connection.ConnectionString.Dump()
        use cn = new SqlConnection(cs)
        let sc = Microsoft.SqlServer.Management.Common.ServerConnection(cn)
        let server = new Microsoft.SqlServer.Management.Smo.Server(sc)
        server.ConnectionContext.Connect()
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
                    |> server.ConnectionContext.ExecuteNonQueryWithCatch
                    |> ignore<int>
                    lastRun <- reKeyed
                    //run <- run + 1
                    runValue.OnNext(runValue.Value + 1)
                with ex -> 
                    lastRun.Dump("last completed operation")
                    (lines,setVars,ex).Dump()
                    let exeScalar (s:string) = server.ConnectionContext.ExecuteScalar s
                    ex.Data.Add("@@servicename", exeScalar "SELECT @@servicename")
                    ex.Data.Add("@@servername", exeScalar "select @@servername")
                    ex.Data.Add("db_name", exeScalar "select DB_NAME()")
                    ex.Data.Add("user_name", exeScalar "select user_name()")
                    ex.Data.Add("suser_name", exeScalar "select suser_name()")
                    
                    
                    reraise()
            )
        countValue.OnCompleted()
        runValue.OnCompleted()
        //progressValue.OnCompleted()
        
        lastRun.Dump("last completed operation")
        //(run,count).Dump("run, counted sub-scripts")
        
open Macros.MsBuild
open SqlMgmt
open SqlCmdExe

type DeployBehavior = 
    | BuildThenUseSqlPackageExe
    | BuildThenUseSqlPackageExeWithPreCompare
    | UseSqlPackageExeWithPreCompare
    | GetSqlPackageCommandLine
    //| RunSmo 


open SqlHelpers
open Railways
let outFolder = @"C:\TFS\PracticeManagement\dev\PracticeManagement\bin\sql"
    
outFolder.Dump("dacpac folder")
let getSqlPackageCommandLine dbProjectOutputName includeTransactionalScripts = 
    let cmd,args = 
            let targetDacPac = System.IO.Path.Combine(outFolder, dbProjectOutputName)
            let targetBehavior = TargetBehavior.ConnectionString cs
            cmdLine targetDacPac DoNotDropObjectsNotInSource true Environment.MachineName targetBehavior includeTransactionalScripts
    cmd,args
    
let runDeploy dbName deployBehavior = 
    "running deploy".Dump()
    
    // sql output folder and app output folder are now one and the same
    let projFolder = @"C:\TFS\PracticeManagement\dev\PracticeManagement\Db"
    
    let runSqlPackageExe includeTransactionalScripts = 
        let cmd,args = 
            let targetDacPac = System.IO.Path.Combine(outFolder, dbName)
            let targetBehavior = TargetBehavior.ConnectionString cs
            cmdLine targetDacPac DoNotDropObjectsNotInSource true Environment.MachineName targetBehavior includeTransactionalScripts
        (Environment.CurrentDirectory, cmd,args).Dump("SqlPackageSetup")
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
                let _,errors = Macros.ProcessMacros.runProcSync cmd args None (Some (fun isError msg -> fBoth isError msg; liveMessageStream.OnNext msg)) None None
                Util.OnDemand("Full output markdown", fun() -> displayText ([| allOutsAsMarkdown.ToString() |]) "Markdown").Dump()
                if errors.Any() then
                    Failure (bonafideErrors |> List.ofSeq)
                else Success ()
        with ex -> 
                ex.Message.Dump()
                displayText [| ex.Message |] "Failure"
                liveMessageStream.OnError(ex)
                reraise()
                
                
    //retry all but the fOnErrorsOpt    
    let useSqlPackageExe includeTransactionalScripts = 
        let result = runSqlPackageExe includeTransactionalScripts
        match result with 
        |Success _ -> ()
        |Failure errorList ->
            if errorList.Any() then
                        
                let blocked72031Regex = "Error SQL72031: This deployment may encounter errors during execution because changes to (.*) are blocked by (.*)'s dependency in the target database."
                let trygetObjectR rawErrorLine : Railway<ObjectReference,string list> = // return Seq<Some sprocInfo> so we can say if all errors are sproc errors
                    let getObjectRef fullname =
                        let schema, sprocName = fullname |> afterOrSelf "[" |> before "." |> beforeOrSelf "]", fullname |> after "." |> beforeOrSelf "]" |> afterOrSelf "["
                        ObjectReference.TryCreate schema sprocName
                    match Regex.Match(rawErrorLine, blocked72031Regex) with
                    | reg when reg.Success -> 
                        getObjectRef reg.Groups.[2].Value
                        |> function 
                            |Success violated -> 
                                //(rawErrorLine, violated).Dump("tryGetObjRef success")
                                Success violated
                            | Failure getObjectError -> Failure [ rawErrorLine; getObjectError]
                    | _ -> Failure ["did not match Regex";rawErrorLine]
                    //|> fun h -> rawErrorLine,h
                
                let getisHandleable2 = bind getIsHandleable
                // should include all errors, handleable and not
                let areErrorsHandleableRail : Railway<ObjectReference*Handleable,_> list =
                    errorList
                    |> Seq.map (trygetObjectR >> bind getIsHandleable)
                    |> List.ofSeq
                areErrorsHandleableRail.Dump()
                ()
                if areErrorsHandleableRail |> Seq.forall isSuccess then
                    let handleableErrors = //: (ConflictedObjectReference*Handleable) list = 
                        areErrorsHandleableRail 
                        |> Seq.map (bind2 Some (fun _ -> None)) 
                        |> Seq.choose id 
                        
                        |> List.ofSeq
                    let errorManagersRail = handleableErrors |> Seq.map (fun (r,h) -> r,h,getManagerRail r h) |> List.ofSeq
                    
                        // drop/disablers with exist clauses
                    errorManagersRail 
                    |> Seq.map (fun (_,_, managerRail) -> match managerRail with | Success m -> m.Drop | Failure s -> failwithf "%s" s)//) match handler with | Trigger -> getDisableTriggerWithExists cor.Violated od |Sproc -> getDropSprocText cor.Violated)
                    |> fun s -> String.Join("\r\nGO\r\n\r\n",s)
                    |> dumpt "disable/drops"
                    |> ignore

                    Util.OnDemand("errorManagersRail", fun () -> errorManagersRail) |> Dump |> ignore
                    Util.OnDemand("handleableErrors", fun () -> handleableErrors) |> Dump  |> ignore
                    ()
                    //failwithf "No options for dealing with handleable errors %A" handleableErrors               
                    ()
                else
                    errorList.Dump()
                        //errors.Dump("errors found")
                        //displayText (errors |> Array.ofSeq) "Errors"
                
    // avoiding any possibility of infinite recursion
    
    
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
    let useTransactionalScripts = true
    let useSqlPackageExe () = 
            //useSqlPackageWithSprocRetry()
            useSqlPackageExe useTransactionalScripts 
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
    | GetSqlPackageCommandLine ->
        getSqlPackageCommandLine dbName useTransactionalScripts 
        |> printfn "CommandLine: %A"
        
//    | RunSmo ->
//        SqlMgmt.migrate @"C:\TFS\PracticeManagement\dev\PracticeManagement\Db\sql\debug\PmMigration.publish.sql" dbName
//

// ***********************************************************************
// this section requires a linqpad connection, commented out so it will work without one
//let checkForInvalidCharges () = 
//    let invalidCharges = 
//        dc.ExecuteQuery<Charges> @"
//select c.*
//from dbo.charges c 
//left join dbo.appointments a 
//    on c.appointmentid = a.appointmentid
//where a.appointmentid is null"
//        |> List.ofSeq
//    invalidCharges.Dump("invalid charges")
//
//    failwithf "Invalid charges, can not deploy or process"
//
//let checkForInvalidPaymentTypes () = 
//    let invalidPayments = 
//        dc.ExecuteQuery<Payments> @"select * from payments where paymenttype = 0"
//        |> List.ofSeq
//    invalidPayments.Dump("invalid payments")
//    failwithf "Invalid payments, can not deploy or process"
//    
//type LargestPaymentInfo () = 
//    member val PaymentID = 0 with get,set
//    member val Amount = 0m with get,set
//    member val DataLength = 0 with get,set
//    member val Length = 0 with get,set
//    
//let getLargestPayment () = 
//    dc.ExecuteQuery<LargestPaymentInfo> @"select len(amount) as length, DATALENGTH(amount) as datalength, * 
//    from payments p
//    where p.amount = (select max(amount) from payments)"
//    |> Seq.tryHead // table could be empty
//    |> function 
//        | Some lpi ->  lpi.Dump()
//        | None -> ()
//        
//getLargestPayment()
// **********************************************************************


let dbProjectOutputName = "PmMigration" // project outputs a PmMigration.dacpac
DateTime.Now.Dump("starting deploy")
//runDeploy dbProjectOutputName BuildThenUseSqlPackageExeWithPreCompare
runDeploy dbProjectOutputName GetSqlPackageCommandLine
//runDeploy dbProjectOutputName UseSqlPackageExeWithPreCompare
DateTime.Now.Dump("finishing deploy")
//runDeploy dbProjectOutputName RunSmo

//Util.Cmd(cmdLine).Dump()