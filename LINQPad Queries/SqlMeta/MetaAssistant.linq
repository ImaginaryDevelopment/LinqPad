<Query Kind="FSharpProgram">
  
</Query>

// purpose: a sql meta api
let dc = new TypedDataContext()
(dc.Connection.DataSource,dc.Connection.Database).Dump()
let after (d:string) (s:string) = s.Substring( s.IndexOf(d) + d.Length) 
let trim (s:string) = s.Trim()
let dumpt (t:string) x = x.Dump(t); x
let startsWithI (d:string) (s:string) = s.StartsWith(d, StringComparison.InvariantCultureIgnoreCase)
type Schema = string
type TableName = string

type MetaCommand = 
    | DropTable of Schema*TableName
    | DropProcedure of string
    | RenameTable of Schema*TableName*TableName
    | SpHelp of string
    | SpHelpText of string
    | RunClipboardCommand
    | RunClipboardQuery
    | AlterSprocFromCreate

let command = RunClipboardQuery

let runCommand =
    let getClipboardText() = System.Windows.Forms.Clipboard.GetText()
    function
    | DropTable (s,t) ->
        dc.ExecuteCommand(sprintf "drop table %s.%s" s t).Dump()
    | DropProcedure n ->
        dc.ExecuteCommand(sprintf "drop procedure %s" n).Dump()
    | RenameTable(s,t,newTableName) ->
        dc.sys.sp_rename(sprintf "%s.%s" s t, newTableName, null).Dump()
    |SpHelpText name -> 
        dc.sys.sp_helptext(name,null).Dump()
    |RunQuery -> 
        getClipboardText()
        |> fun t -> dc.ExecuteQueryDynamic(t).Dump()
        |> ignore
        
    |RunClipboardCommand ->
        let text = System.Windows.Forms.Clipboard.GetText()
        dc.ExecuteCommand(text).Dump()
    | AlterSprocFromCreate -> 
        let text = System.Windows.Forms.Clipboard.GetText()
        if text |> string |> trim |> startsWithI "create" |> not then
            text.Dump("doesn't appear to be a sproc create statement")
            failwithf "Unable to process text"
        text.Dump("create from clipboard")
        text 
        |> after "CREATE "
        |> sprintf "%s %s" "ALTER "
        |> dumpt "ALTER statment"
        |> dc.ExecuteCommand
        |> Dump
        |> ignore
        
runCommand command
