<Query Kind="FSharpProgram">
  
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