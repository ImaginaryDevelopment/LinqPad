<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft SQL Server\100\SDK\Assemblies\Microsoft.SqlServer.ConnectionInfo.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft SQL Server\100\SDK\Assemblies\Microsoft.SqlServer.ConnectionInfoExtended.dll</Reference>
  <GACReference>Microsoft.SqlServer.Instapi, Version=10.0.0.0, Culture=neutral, PublicKeyToken=89845dcd8080cc91</GACReference>
  <NuGetReference>System.Reactive</NuGetReference>
  <Namespace>Microsoft.SqlServer.Management.Common</Namespace>
  <Namespace>Microsoft.SqlServer.Management.Trace</Namespace>
  <Namespace>System.Reactive.Subjects</Namespace>
</Query>

let dumpt (t:string) x = x.Dump(t); x
let after (delimiter:string) (x:string) = 
    match x.IndexOf delimiter with
    | i when i < 0 -> failwithf "after called without matching substring in '%s'(%s)" x delimiter
    | i -> x.Substring(i + delimiter.Length)
let before (delimiter:string) (x:string) = x.Substring(0,x.IndexOf(delimiter))
let contains (delimiter:string) (x:string) = x.Contains(delimiter)
let beforeOrSelf delimiter x = if x |> contains delimiter then x |> before delimiter else x

type TestedGoodConnectionType = 
    | Trusted of serverNameOpt:string option*databaseNameOpt:string option // uses localhost default db trusted connection if the options aren't filled out
    | Untrusted of server:string*username:string*password:string

let (|TrustedText|UntrustedText|) (s:string) = 
    match s.Contains("Integrated Security=SSPI"), s.Contains("Data Source") with
    | true,true -> TrustedText
    | false,true -> UntrustedText
    | _ -> failwithf "No implemented parser for %s" s
    
let parseCString input = 
    let getTerm key = input |> after (sprintf "%s=" key) |> beforeOrSelf ";"
    input.Dump("parsing")
    match input with
    | TrustedText -> 
        let ds = getTerm "Data Source"
        let db = getTerm "Initial Catalog"
        Trusted(Some ds,Some db)
    | UntrustedText -> 
        let server = input |> after "Data Source=" |> beforeOrSelf ";"
        let userName = input |> after "User ID=" |> dumpt "after UserID" |> beforeOrSelf ";"
        let pwd = input |> after "Password=" |> beforeOrSelf ";"
        Untrusted(server, userName, pwd)
    
//dc.Connection.ConnectionString.Dump()


let testedGood = 
    [   
        //parseCString dc.Connection.ConnectionString
        Trusted(None,None)
    ]
    
testedGood.Dump("testedgood")

let ci = 
    let inline setDbNameOpt dbNameOpt (ci:SqlConnectionInfo) = 
        match dbNameOpt with
        | Some dbName -> ci.DatabaseName <- dbName
        | None -> ()
        ci
    match testedGood.[0] with
    | Untrusted (server,username,password) -> SqlConnectionInfo(server, username, password)
    | Trusted (Some serverName, dbNameOpt) -> 
        SqlConnectionInfo(serverName,UseIntegratedSecurity=true)
        |> setDbNameOpt dbNameOpt
        
    | Trusted (None, dbNameOpt) ->
        SqlConnectionInfo(UseIntegratedSecurity=true)
        |> setDbNameOpt dbNameOpt

//ci.UseIntegratedSecurity <- true
//ci.ServerName <- "localhost"
//ci.DatabaseName <- "PROD"
let testConnectionInfo () = 
    use cn = ci.CreateConnectionObject()
    cn.Open()
    cn.Dump()
    cn.Dispose()
IntPtr.Size.Dump()
module Subjects = 
    let inline setupSubject (name:string) (defaultValue:'t) = 
        let subject = new BehaviorSubject<'t>(defaultValue)
        let _ = subject.DumpLatest(name)
        subject
open Subjects

let runTrace () = 
    
    use lastEventClass = setupSubject<string option> ("lastEvent") (None)
    // TODO: rotate these into a table or observable object perhaps
    use lastBatch = setupSubject<(string*int)>("lastBatch") (null,0)
    use stmtStarted = setupSubject<(string*int)>("stmtStarted") (null,0)
    use stmtCompleted = setupSubject<(string*int)>("stmtCompleted") (null, 0)
    use trace = new TraceServer()
    let tf = 
        // first two are confirmed functional
        //@"C:\projects\LinqPad\LinqPad\LINQPad Queries\standard.tdf"
        //@"C:\projects\LinqPad\LinqPad\LINQPad Queries\example.tdf"
        @"C:\projects\LinqPad\LinqPad\LINQPad Queries\mine.tdf"
        //@"C:\Program Files (x86)\Microsoft SQL Server\100\Tools\Profiler\Templates\Microsoft SQL Server\1050\TSQL.tdf"
    if not <| IO.File.Exists tf then raise <| invalidOp "File not found"
//SELECT ISNULL(SESSIONPROPERTY ('ANSI_NULLS'), 0), ISNULL(SESSIONPROPERTY ('QUOTED_IDENTIFIER'), 1)
    let mutable pauseForUI = false
    trace.InitializeAsReader(ci, tf)
    try
        while(trace.Read()) do
            if pauseForUI then
                System.Threading.Thread.Sleep(1000) // try to let ui catch up
            //"dumping a trace!".Dump()
            let getValueStrOrNull i = 
                trace.GetValue i
                |> fun v -> if isNull v then null else v |> string
            let rowDict = [0..trace.FieldCount-1] |> Seq.map(fun i -> trace.GetName(i), getValueStrOrNull i) |> dict
            lastEventClass.OnNext(Some "EventClass")
            try
                match rowDict.["EventClass"] with
                | "Trace Skipped Records" -> ()
                | "ExistingConnection" as x -> Util.OnDemand(x, fun () -> rowDict).Dump()
                | "SQL:BatchStarting" when rowDict.ContainsKey "TextData" -> 
                    match rowDict.["TextData"] with
                    |null 
                    |"" -> ()
                    | "SELECT ISNULL(SESSIONPROPERTY ('ANSI_NULLS'), 0), ISNULL(SESSIONPROPERTY ('QUOTED_IDENTIFIER'), 1)" -> () // ignore spammy message
                    | x when x.Length = 138 && x.TrimEnd().EndsWith("SET NUMERIC_ROUNDABORT OFF;") -> () // ignore spammy message
                    |x when x.Length > 4000 -> 
                        // didn't function
                        //let tryGetFullTextLink = Util.OnDemand("full text?", fun() -> pauseForUI <- true; x)
                        (x.Substring(0,1000), x.Length) |>  lastBatch.OnNext
                    |x -> lastBatch.OnNext(x,x.Length)
                    () 
                    // swallowing for now if needed uncomment the following
                    //Util.OnDemand("Huge statement",fun () -> rowDict
                | "SQL:StmtStarting" when rowDict.ContainsKey "TextData" -> 
                    match rowDict.["TextData"] with
                    | null
                    |"" -> ()
                    | x -> stmtStarted.OnNext(x,x.Length)
                    ()
                | "SQL:StmtCompleted" when rowDict.ContainsKey "TextData" ->
                    match rowDict.["TextData"] with
                    | null
                    |"" -> ()
                    | x -> stmtCompleted.OnNext(x,x.Length)

                | _ -> 
                    ()
                    //rowDict.Dump()
            with ex -> 
                rowDict.Dump("traceItems!")    
            ()
            
            //trace.FieldCount.Dump("trace")
    finally 
        trace.Stop()
        trace.Close()
runTrace()

//typeof<Microsoft.SqlServer.Management.Trace.>.Assembly.DefinedTypes
//|> Dump