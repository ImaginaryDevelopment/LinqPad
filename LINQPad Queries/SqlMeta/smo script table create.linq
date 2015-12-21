<Query Kind="FSharpProgram">
  <Connection>
    <ID>3da1b433-c8cb-407a-9c25-1d4f2ea04d64</ID>
    <Persist>true</Persist>
    <Server>192.168.0.187</Server>
    <SqlSecurity>true</SqlSecurity>
    <IncludeSystemObjects>true</IncludeSystemObjects>
    <Database>ApplicationDatabase</Database>
    <ShowServer>true</ShowServer>
  </Connection>
  <GACReference>Microsoft.SqlServer.ConnectionInfo, Version=12.0.0.0, Culture=neutral, PublicKeyToken=89845dcd8080cc91</GACReference>
  <GACReference>Microsoft.SqlServer.Smo, Version=12.0.0.0, Culture=neutral, PublicKeyToken=89845dcd8080cc91</GACReference>
</Query>

let dc = new TypedDataContext()

open Microsoft.SqlServer.Management.Common
open Microsoft.SqlServer.Management.Smo
let conn = dc.Connection :?> SqlConnection
conn.Dump()
conn.Open()
Server(ServerConnection(conn))
|> (fun s -> s.Databases.["ApplicationDatabase"])
|> (fun d -> d.Tables.["Charge","dbo"])
|> (fun t -> t.Script(ScriptingOptions(SchemaQualify = true,DriAll = true)))
//|> Seq.cast<String> 
|> Dump