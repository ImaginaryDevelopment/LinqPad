<Query Kind="FSharpProgram">
  
</Query>

// read a table and generate inserts from it
let dc = new TypedDataContext()

let startsWith d (s:string) = s.StartsWith d
let replace d (t:string) (s:string) = s.Replace(d, t)
let delimit d (items :string seq ) = String.Join(d,items |> Array.ofSeq)
let flip f y x = f x y
let t = typeof<TempLanguages>
let target = 

    
    let persistent = 
        dc.Mapping.GetTable(typeof<TempLanguages>).RowType.PersistentDataMembers 
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
        
let mapValue t (canNull:bool) (x:obj) =
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
let valuesMap members (x:TempLanguages) = 
        members 
        |> Seq.map (fun (name:string,t:Type, nullable:bool, getter:obj -> obj) -> getter x |> mapValue t nullable) 
        |> List.ofSeq
        |> delimit ","
let generateInserts tableName (rowType:Type) columnBlacklist (members:#seq<string*Type*bool*(obj -> obj)>) = 
    
    let insertStatement tableName columns= sprintf "insert into %s(%s) values (" tableName columns
    let members = members |> Seq.filter(fun (name,_,_,_) -> columnBlacklist |> Seq.exists (fun b -> b = name) |> not) |> List.ofSeq
    let columns = members |> Seq.map (fun (name,_,_,_) -> name) |> delimit ","
        
    let insert:string = insertStatement tableName columns
    
    dc.TempLanguages
    |> Seq.map (valuesMap members >> sprintf "%s%s)" insert)
    

let generateMyInserts() = 
    generateInserts "Language" t ["LanguageID"] target
    |> Dump

let generateMerge (schema:string) (table:string) joiner isIdentity (members:#seq<string*Type*bool*(obj -> obj)>)  = 
    let columns = members |> Seq.map (fun (name,_,_,_) -> name) |> delimit ","
    let valuesMap (x:TempLanguages) = 
        members 
        |> Seq.map (fun (name,t, nullable, getter) -> getter x |> mapValue t nullable) 
        |> List.ofSeq
        |> delimit ","
    let starter1 = 
        sprintf """
---------------------------------------------------
PRINT 'Synchronizing [%s.%s]';""" schema table
    let starter2:string = if isIdentity then sprintf "%sset identity_insert %s.%s on%s" Environment.NewLine schema table Environment.NewLine else null
// -----------------------------------------
    let starter3 = sprintf """
WITH CTE_%s(%s) AS
(
    SELECT %s
    FROM (VALUES
""" 
    let starter3 = starter3 table columns columns
    let allValues = 
        dc.TempLanguages
        //|> Seq.take 10
        |> Seq.map valuesMap
        |> Seq.map (fun s -> "        (" + s + ")") 
        |> delimit ("," + Environment.NewLine)
    let closer1 = sprintf """    )
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
        printout]
    |> Seq.fold (fun sb l -> append l sb) (StringBuilder())
    |> string
    
    
//PRINT 'Done Synchronizing [<#= schema #>.<#= table #>]';
//GO"" schema
////dc.TempLanguages
////|> Seq.map (fun l -> sprintf "Insert into
generateMerge "dbo" "Language" "LanguageID" true target 
|> Dump