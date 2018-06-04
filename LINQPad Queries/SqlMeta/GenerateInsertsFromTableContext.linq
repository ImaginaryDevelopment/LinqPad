<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
</Query>

// read a table and generate inserts from it
let dc = new TypedDataContext()
module Helpers =
    let startsWith d (s:string) = s.StartsWith d
    let replace d (t:string) (s:string) = s.Replace(d, t)
    let delimit d (items :string seq ) = String.Join(d,items |> Array.ofSeq)
    let flip f y x = f x y
    let private failNullOrEmpty paramName x = if String.IsNullOrEmpty x then raise <| ArgumentOutOfRangeException paramName else x
    type System.String with
        static member defaultIComparison = StringComparison.InvariantCultureIgnoreCase
        static member equalsI (x:string) (x2:string) = not <| isNull x && not <| isNull x2 && x.Equals(x2, String.defaultIComparison)
        static member substring i (x:string) = x.Substring i
        static member substring2 i e (x:string)= x.Substring(i,e)
        static member before (delimiter:string) s = s |> String.substring2 0 (s.IndexOf delimiter)
        static member after (delimiter:string) (s:string) = 
            failNullOrEmpty "delimiter" delimiter |> ignore
            match s.IndexOf delimiter with
            | i when i < 0 -> failwithf "after called without matching substring in '%s'(%s)" s delimiter
            | i -> s |> String.substring (i + delimiter.Length)
open Helpers
let mapTableName = replace "[" "" >> replace "]" ""
dc.Mapping.GetTables()
|> Seq.map(fun t -> t.TableName |> mapTableName) //,t.RowType.Type, t.RowType.DataMembers)
|> Dump
|> ignore

//dc.Mapping.MappingSource.GetModel(typeof<TypedDataContext>).GetMet

//let tableName =  "Accidents"
let targetTable,schemaName,tableName = 
    let tables =  dc.Mapping.GetTables()
    let tableNames = 
        tables
        |> Seq.map(fun t -> t.TableName |> mapTableName)
        |> List.ofSeq
    let user = Util.ReadLine("Table?",null,tableNames)
    let tt = 
        tables
        |> Seq.tryFind(fun t -> t.TableName |> mapTableName |> String.equalsI user) 
        |> function
            | Some t -> t
            | None -> failwithf "Unable to find table %s" user
    // adjust user case insensitive search to case specific name
    let user = tt.TableName |> mapTableName
    if user.Contains "." then
        tt, user |> String.before ".", user |> String.after "."
    else tt,"dbo",user
Util.ClearResults()    
printfn "Running generation for table %s.%s" schemaName tableName
//type Target = Accidents

let values = dc.GetTable(targetTable.RowType.Type) |> Seq.cast<obj>

//let schema = "dbo"

//let identityColumnName = "AccidentID"

let t = targetTable.RowType.Type //typeof<Target>
let joiner,isIdentity = 
    match targetTable.RowType.DataMembers |> Seq.filter(fun dm -> dm.IsPrimaryKey) |> List.ofSeq with
    | [] -> failwithf "No primary key(s) found"
    | pk1::pk2::_ -> raise <| NotImplementedException( "Multiple primary keys not implemented")
    | pk::[] -> pk.Name, pk.IsDbGenerated
let target = 

    let persistent = 
        try
            dc.Mapping.GetTable(t).RowType.PersistentDataMembers 
        with _ ->
            t.Name.Dump("Failing PersistentDataMembers")
            reraise()
        |> Seq.filter(fun pm -> 
//            printfn "Column %s is Association? %A" pm.Name pm.IsAssociation
            not pm.IsAssociation
        )
        |> Seq.map (fun pm -> pm.Name, pm.Type, pm.CanBeNull,t.GetField(pm.Name))
        |> List.ofSeq
        //|> Dump
        |> Seq.map (fun (name,t, n, getter) -> name,t, n, fun x -> getter.GetValue x)
        |> List.ofSeq
    persistent
    
[<AutoOpen>]
module TypeMapper = 
    let (|TypeDefOf|_|) (_:'a) t = 
        if t = typedefof<'a> then Some() else None
    let (|TypeOf|_|) (_:'a) t = 
        if t = typeof<'a> then Some ()
        else 
            //printfn "did not match %A to %A" typeof<'a> t ; 
            None

    let isType<'a> = Unchecked.defaultof<'a>

    let mapTypeToF = 
        function
        | TypeOf(isType:int) -> "int"
        | TypeOf(isType:string) -> "string"
        | TypeOf(isType:Nullable<DateTime>) -> "DateTime Nullable"
        | TypeOf(isType:Nullable<int>) -> "int Nullable"
        | TypeOf(isType:Nullable<bool>) -> "bool Nullable"
        | x -> x.Name
        
let mapValue (t:Type) (canNull:bool) (x:obj) =
    match x with
    | null -> "null"
    | _ -> 
        match t with
        | TypeOf(isType:string) -> sprintf "'%s'" (x |> string |> replace "'" "''")
        | TypeOf(isType:bool) -> if x :?> bool then "1" else "0"
        | TypeOf(isType:Nullable<bool>) -> 
            let v = x :?> Nullable<bool>
            match v.HasValue with
            | true-> if v.Value then "1" else "0"
            | false -> "null"
        | _ -> x |> string
        
//type InsertStrategy =
//    |RowByRow
//    |Merge

let generateInserts tableName (rowType:Type) columnBlacklist (members:#seq<string*Type*bool*(obj -> obj)>) = 
    let valuesMap members (x:obj) = 
        members 
        |> Seq.map (fun (name:string,t:Type, nullable:bool, getter:obj -> obj) -> getter x |> mapValue t nullable) 
        |> List.ofSeq
        |> delimit ","
    let insertStatement tableName columns= sprintf "insert into %s(%s) values (" tableName columns
    let members = members |> Seq.filter(fun (name,_,_,_) -> columnBlacklist |> Seq.exists (fun b -> b = name) |> not) |> List.ofSeq
    let columns = members |> Seq.map (fun (name,_,_,_) -> name) |> delimit ","
        
    let insert:string = insertStatement tableName columns
    
    values
    |> Seq.map (valuesMap members >> sprintf "%s%s)" insert)
    

let generateMyInserts() = 
    generateInserts tableName t [joiner] target
    |> Dump

let generateMerge (schema:string) (table:string) joiner isIdentity (members:#seq<string*Type*bool*(obj -> obj)>)  = 
    let identitySetter on = if isIdentity then sprintf "%sset identity_insert %s.%s %s;%s" Environment.NewLine schema table (if on then "on" else "off") Environment.NewLine else null
    let columns = members |> Seq.map (fun (name,_,_,_) -> name) |> delimit ","
    let valuesMap (x:obj) = 
        members 
        |> Seq.map (fun (name,t, nullable, getter) -> 
            try
                getter x |> mapValue t nullable
            with _ ->
                printfn "Failing for %s %A" name t.FullName
                reraise()
        ) 
        |> List.ofSeq
        |> delimit ","
    
    let starter1 = 
        sprintf """
---------------------------------------------------
PRINT 'Synchronizing [%s.%s]';""" schema table
    let starter2:string = identitySetter true
// -----------------------------------------
    let starter3 = sprintf """
WITH CTE_%s(%s) AS
(
    SELECT %s
    FROM (VALUES
""" 
    let starter3 = starter3 table columns columns
    let allValues = 
        values
        //|> Seq.take 10
        |> Seq.map valuesMap
        |> Seq.map (fun s -> "        (" + s + ")") 
        |> delimit ("," + Environment.NewLine)
    let closer1 = sprintf """
    )
    AS SOURCE(%s)
)
MERGE INTO [%s].[%s] AS TARGET
USING CTE_%s""" 

// ---------------------------------------------------------------------------
    let closer1 = closer1 columns schema table table
    let closer2 = sprintf """
    ON CTE_%s.[%s] = TARGET.[%s]
WHEN NOT MATCHED BY TARGET THEN
    INSERT(%s)
    VALUES(%s);
    """
// -------------------------------------------------------------------------
    let closer2 = closer2 table joiner joiner columns columns
    let closer3 = identitySetter false
    let printout = sprintf """
    PRINT 'Done Synchronizing [%s.%s]';
GO""" 
    let printout = printout schema table
    let append (text:string) (sb:StringBuilder) = sb.Append(text)
    
    
    [   starter1
        starter2
        starter3
        allValues
        closer1
        closer2
        closer3
        printout]
    |> Seq.fold (fun sb l -> append l sb) (StringBuilder())
    |> string
    
    
//PRINT 'Done Synchronizing [<#= schema #>.<#= table #>]';
//GO"" schema
////dc.TempLanguages
////|> Seq.map (fun l -> sprintf "Insert into
// "dbo" "Language" "LanguageID" 
let text = generateMerge schemaName tableName joiner isIdentity target 
let rec refreshLinks() = 
    let eagerDumpLimit,dumpTextLimit = 1000, 2000
    Util.ClearResults()
    [        
        "Copy to clipboard", fun () ->
            System.Windows.Forms.Clipboard.SetText text
            "Saved to clipboard"
        "Save to file", fun () ->
            use sfd = new System.Windows.Forms.SaveFileDialog()
            match sfd.ShowDialog() = System.Windows.Forms.DialogResult.OK with
            | true ->
                File.WriteAllText(sfd.FileName,text)
                sprintf "Saved to file %s" sfd.FileName
            | _ -> "Save cancelled"
//        "Dump", fun () -> text
        // useful if you need the copy to clipboard link to work again, or the others
        "Refresh this menu", (fun () -> refreshLinks(); "Refreshed")
    ] |> Seq.iter (Util.OnDemand >> Dump >> ignore)
    if text.Length > dumpTextLimit then
        printfn ""
        printfn "Too big to dump entire output"
        printfn ""
    // desired feature: try to clip near EOL instead of just anywhere.
    if text.Length < eagerDumpLimit then
        text.Dump()
    else
        text
        |> Seq.truncate 500
        |> Array.ofSeq
        |> String
        |> Dump
        |> ignore
        
        if text.Length > 500 then
            printfn ""
            if text.Length < dumpTextLimit then
                let endSnipContainer = DumpContainer()
                let f :unit -> unit = fun () -> endSnipContainer.Content <- box ""
                Util.OnDemand("...",fun () -> f(); text.[500..]).Dump()
                printfn ""
                endSnipContainer.Content <- box text.[text.Length-100..]
                endSnipContainer.Dump()
            else 
                printfn "..."
                printfn ""
                text.[text.Length - 300 ..]
                |> printfn "%s"
refreshLinks()
