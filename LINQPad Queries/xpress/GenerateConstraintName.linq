<Query Kind="FSharpProgram" />

// create fkey name for table column

let trim (s:string) = if isNull s then null else s.Trim()
let after (delimiter:string) (s:string) = s.Substring(s.IndexOf delimiter + delimiter.Length)
let before (delimiter:string) (s:string) = s.Substring(0,s.IndexOf delimiter)
let beforeI (delimiter:string) (s:string) = s.Substring(0,s.IndexOf(delimiter, StringComparison.CurrentCultureIgnoreCase))
let beforeIOrSelf (delimiter:string) (s:string) = if s.Contains delimiter then s |> beforeI delimiter else s
let capture (pattern:string) (s:string) = Regex.Match(s, pattern).Groups.[1].Value

// assume tablename matches with file (e.g. Payment.Table.sql) and is properly cased
let getTableName filename = Path.GetFileNameWithoutExtension(Path.GetFileNameWithoutExtension(filename))
let dbProjPath = @"C:\TFS\PracticeManagement\dev\PracticeManagement\Db" 
let sqlTableFiles = 
    Directory.GetFiles(dbProjPath, "*.table.sql", SearchOption.AllDirectories)
    |> Array.map(fun fn -> getTableName fn, fn)
let tryFindTableName columnName = // assume column name ends with ID
    columnName
    |> beforeIOrSelf "ID"
    |> (fun name -> sqlTableFiles |> Seq.tryFind(fun (tn,_) -> String.Equals(tn,name, StringComparison.CurrentCultureIgnoreCase)))
    |> function
        | Some (tableName,_) -> tableName
        | None -> null

let readColumns (lines:#seq<string>) = 
    lines
    |> Seq.map trim
    |> Seq.filter (fun s-> s.StartsWith("["))    
    |> Seq.map (fun s-> s |> after "[" |> before "]", s |> after "]" |> capture @"\s+(\w+\s*(\(([0-9, ]+|MAX)\))?)\s*", s)
    |> Seq.map (fun (columnName, columnType, line) -> columnName,columnType,line |> after columnType |> trim)
    //|> Seq.map (fun (columnName, columnType, constraints)
    |> Array.ofSeq
    
let target = @"C:\TFS\PracticeManagement\devDb\Schema Objects\Schemas\dbo\Tables\Payment.table.sql" 

let tableName = getTableName target
tableName.Dump("tablename")
// assume 1 column per line
let columns = 
    File.ReadAllLines target
    |> readColumns

columns.Dump("Columns")
let targetColumn = Util.ReadLine("Column to key from?", null, columns |> Seq.map (fun (cn,_,_) -> cn))

let fkeyTable,fKeyFilename = 
    let defaultValue = tryFindTableName targetColumn
    Util.ReadLine("Table to key to?", defaultValue , sqlTableFiles |> Seq.map fst) 
    |> (fun tbl -> sqlTableFiles |> Seq.find(fun (tableName,_) -> tableName = tbl))
let fkeyColumn = 
    let columnSuggestions : string IEnumerable = 
        let columnTuples = File.ReadAllLines fKeyFilename |> readColumns
        columnTuples.Dump("target columnInfo")
        columnTuples 
        |> Seq.map(fun (cn,_,_) -> cn)
    let defaultValue = fkeyTable + "ID"
    Util.ReadLine(prompt="Column to key to?", defaultValue=defaultValue, suggestions=columnSuggestions )
//TODO: account for non-dbo schema tables
sprintf "CONSTRAINT [FK_%s_%s_%s_%s] FOREIGN KEY ([%s]) REFERENCES [dbo].[%s] ([%s])" tableName targetColumn fkeyTable fkeyColumn targetColumn fkeyTable fkeyColumn
|> fun s-> s.Dump("constraint")
