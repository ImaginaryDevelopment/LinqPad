<Query Kind="FSharpProgram">
  <Connection>
    <ID>4e94eacc-a31d-4687-947b-e4c9804c895a</ID>
    <Persist>true</Persist>
    <Server>(local)</Server>
    <IncludeSystemObjects>true</IncludeSystemObjects>
    <Database>PmRewriteApplicationDatabase</Database>
    <ShowServer>true</ShowServer>
  </Connection>
</Query>

let dc = new TypedDataContext()
dc.Connection.Open()
//get transaction level
use cmd = dc.Connection.CreateCommand()
cmd.CommandText <- """SELECT CASE transaction_isolation_level 
WHEN 0 THEN 'Unspecified'
WHEN 1 THEN 'ReadUncommitted'
WHEN 2 THEN 'ReadCommitted'
WHEN 3 THEN 'Repeatable'
WHEN 4 THEN 'Serializable'
WHEN 5 THEN 'Snapshot' END AS TRANSACTION_ISOLATION_LEVEL
FROM sys.dm_exec_sessions
where session_id = @@SPID"""
let isolationLevel = cmd.ExecuteScalar()
isolationLevel.Dump("isolationLevel")

if isolationLevel.ToString() <> "ReadUncommitted" then
    cmd.CommandText <-"""SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED"""
    cmd.ExecuteNonQuery() |> ignore