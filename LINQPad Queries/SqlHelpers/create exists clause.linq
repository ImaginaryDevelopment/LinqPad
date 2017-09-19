<Query Kind="FSharpProgram" />

// create if exists clause for sql script

type Condition = 
    | ColumnExists of schema:string*table:string*column:string
let createExists =
    function
    | ColumnExists (schema,table,column) -> 
        sprintf "if exists( select 1 from INFORMATION_SCHEMA.columns c where c.TABLE_SCHEMA='%s' and c.TABLE_NAME='%s' and c.COLUMN_NAME = '%s')\r\nbegin\r\n--\r\nend" schema table column
        
ColumnExists("dbo","Payment","IsElectronic")        
|> createExists
|> Dump
|> ignore