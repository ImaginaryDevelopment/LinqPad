<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft SQL Server\100\SDK\Assemblies\Microsoft.SqlServer.ConnectionInfo.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft SQL Server\100\SDK\Assemblies\Microsoft.SqlServer.ConnectionInfoExtended.dll</Reference>
  <GACReference>Microsoft.SqlServer.Instapi, Version=10.0.0.0, Culture=neutral, PublicKeyToken=89845dcd8080cc91</GACReference>
  <Namespace>Microsoft.SqlServer.Management.Common</Namespace>
  <Namespace>Microsoft.SqlServer.Management.Trace</Namespace>
</Query>

let dc = new TypedDataContext()

let after (delimiter:string) (x:string) = x.Substring(x.IndexOf delimiter + delimiter.Length)
let before (delimiter:string) (x:string) = x.Substring(0,x.IndexOf(delimiter))
let contains (delimiter:string) (x:string) = x.Contains(delimiter)
let beforeOrSelf delimiter x = if x |> contains delimiter then x |> before delimiter else x

type TestedGoodConnectionType = 
    | Trusted of serverNameOpt:string option*databaseNameOpt:string option // uses localhost default db trusted connection if the options aren't filled out
    | Untrusted of server:string*username:string*password:string

let parseCString input = 
    let server = input |> after "Data Source=" |> beforeOrSelf ";"
    let userName = input |> after "User ID=" |> beforeOrSelf ";"
    let pwd = input |> after "Password=" |> beforeOrSelf ";"
    Untrusted(server, userName, pwd)
    
//dc.Connection.ConnectionString.Dump()


let testedGood = 
    [   parseCString dc.Connection.ConnectionString
        Trusted(None,None)
    ]
    
testedGood.Dump()

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

let runTrace () = 
    use trace = new TraceServer()
    let tf = 
        // first two are confirmed functional
        @"C:\projects\LinqPad\LinqPad\LINQPad Queries\standard.tdf"
        //@"C:\projects\LinqPad\LinqPad\LINQPad Queries\example.tdf"
        //@"C:\Program Files (x86)\Microsoft SQL Server\100\Tools\Profiler\Templates\Microsoft SQL Server\1050\TSQL.tdf"
    if not <| IO.File.Exists tf then raise <| invalidOp "File not found"
    
    trace.InitializeAsReader(ci, tf)
    try
        while(trace.Read()) do
            //"dumping a trace!".Dump()
            let getValueStrOrNull i = 
                trace.GetValue i
                |> fun v -> if isNull v then null else v |> string
            let rowDict = [0..trace.FieldCount-1] |> Seq.map(fun i -> trace.GetName(i), getValueStrOrNull i) |> dict
            try
                match rowDict.["EventClass"] with
                | "ExistingConnection" as x -> Util.OnDemand(x, fun () -> rowDict).Dump()
                | _ -> rowDict.Dump()
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