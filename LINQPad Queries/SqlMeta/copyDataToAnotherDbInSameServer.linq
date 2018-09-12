<Query Kind="FSharpProgram" />

// migrate all data to another db, retry failed ones later
// expects both dbs on same server
let dc = new TypedDataContext()

module Helpers =
    let delimit (d:string) (items:string seq) = String.Join(d,values=items)

()
open Helpers


let sourceDb = "ApplicationDatabase"
let exeNonQuery q = dc.ExecuteCommand(q)
let exeT q = dc.ExecuteQuery(q)
let exe1 q = dc.ExecuteQuery q |> Seq.tryHead
let exeSproc sproc = dc.ExecuteStoredProcedure(sproc)
module Meta = 
    type TableInfo() = 
        member val SchemaName:string = null with get,set
        member val Name:string = null with get,set
    type ColumnInfo() =
        member val Column_Name:string = null with get,set
        member val Is_Nullable:string = null with get,set
        member val Ordinal_Position:int = -1 with get,set
        member val Data_Type:string = null with get,set
        member val IsIdentity:int = -1 with get,set
        
    let tables:(TableInfo) seq = exeT "select schema_name(schema_id) schemaName,name from sys.tables"
    let getTableInfo db schema tName : ColumnInfo seq = 
         sprintf "select *,COLUMNPROPERTY(object_id(TABLE_SCHEMA+'.'+TABLE_NAME), COLUMN_NAME, 'IsIdentity') IsIdentity from %s.information_schema.columns where table_name='%s' and table_schema='%s'" db tName schema
         |> exeT
    let count target: int = sprintf "select count(1) from %s" target |> exe1 |> Option.get
    let identStatement(t:TableInfo) toOn = sprintf "set identity_insert %s.%s %s" t.SchemaName t.Name (if toOn then "on" else "off")
    let generateNonIdentityInsertStatement(t:TableInfo) =  sprintf "insert into %s.%s select * from %s.%s.%s" t.SchemaName t.Name sourceDb t.SchemaName t.Name
    let generateIdentInsertStatement (t:TableInfo) (columns:ColumnInfo seq) = 
        let identOn = identStatement t true
        let identOff = identStatement t false
        let columns = columns |> Seq.map (fun c -> c.Column_Name) |> delimit ","
        let insert = sprintf "insert into %s.%s(%s) select * from %s.%s.%s" t.SchemaName t.Name columns sourceDb t.SchemaName t.Name
        sprintf "%s\r\n%s\r\n%s" identOn insert identOff
    
open Meta
type ColumnInfo with
    member x.IsNullable = x.Is_Nullable = "YES"
let mutable errorCount = 0
let mutable skipCount = 0
let mutable moveCount = 0
let failTexts = ResizeArray()
let fWork = 
    let dc = DumpContainer()
    dc.Dump("status")
    (fun (x:obj) ->
        dc.Content <- null
        dc.Content <- x
    )
tables
|> Seq.iter(fun t ->
    fWork t
    let rowCount = count <| sprintf "%s.%s" t.SchemaName t.Name 
    let srcCount=count <| sprintf "%s.%s.%s" sourceDb t.SchemaName t.Name
    // check for data already in destination
    if rowCount = 0 then
        printfn "%A,%A items in %s" srcCount rowCount t.Name
        let columns = getTableInfo dc.Connection.Database t.SchemaName t.Name |> List.ofSeq
        let hasIdentity = columns |> Seq.exists(fun c -> c.IsIdentity = 1)
        let text = 
            if hasIdentity then
            
                generateIdentInsertStatement t columns
            else generateNonIdentityInsertStatement t
        try
            let valuesMoved = exeNonQuery text
            moveCount <- moveCount + 1
            printfn "moved %i values to %s.%s" valuesMoved t.SchemaName t.Name
        with 
        | ex when ex.Message = "The select list for the INSERT statement contains fewer items than the insert list. The number of SELECT values must match the number of INSERT columns." ->
            let srcColumns = getTableInfo sourceDb t.SchemaName t.Name
            (srcColumns,columns).Dump("less columns than target?")
            ()
        
        |ex -> 
            errorCount <- errorCount + 1
            failTexts.Add(sprintf "%s\r\n%s" text ex.Message)
    else
        skipCount <- skipCount + 1
        
)

(moveCount,skipCount,errorCount).Dump()
failTexts
|> Dump
|> ignore