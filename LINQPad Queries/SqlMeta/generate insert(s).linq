<Query Kind="FSharpProgram" />

let dc = new TypedDataContext()

// begin input

let patientId = 1 // 1360
let patientAccountId = dc.Patients.Where(fun p -> p.PatientID = patientId).Select(fun p -> p.AccountID).First().GetValueOrDefault()
//let target = dc.Statements.First(fun s -> s.StatementID = 231)
type TargetType<'T> =
    |Single of 'T
    |Multiple of 'T list
    
let target = dc.JournalEntries.Where(fun je -> je.CreditAccountID = patientAccountId || je.DebitAccountID = patientAccountId) |> List.ofSeq |> Multiple
let identityIncluded = false
let debug = false


// begin code

type TableAttr = System.Data.Linq.Mapping.TableAttribute
type ColAttr = System.Data.Linq.Mapping.ColumnAttribute

let delimit (d:string) (items:string seq) = String.Join(d,items)

module Seq = 
    let ofType<'t> : _ seq -> 't seq = Seq.choose(box >> function | :? 't as x -> Some x | _ -> None)
    
// get the value and quote it if necessary
let generateColumnInsert (v:obj) : string=
    match v with
    | null -> "null"
    | :? string as x -> x.Replace("'","''") |> sprintf "'%s'"
    | :? DateTime as dt -> dt.ToString("yyyy.MM.dd hh:mm:ss") |> sprintf "'%s'"
    | :? bool as b -> if b then "1" else "0"
    | x -> string x
    
type ColumnType = 
    |Regular
    |Identity
    |Computed
    
let getFieldAttr(p:FieldInfo) = 
    p.GetCustomAttributes()
    // using case, not ofType because I want it to trip if we find a new attribute we haven't accounted for
    |> Seq.cast<ColAttr>
    |> List.ofSeq
    |> function
        | [] -> failwithf "Expected a column attr"
        | x :: [] -> 
            if x.IsDbGenerated && x.IsPrimaryKey then Identity,x
            elif x.IsDbGenerated then Computed,x
            else Regular,x
        
        | _ :: _ :: _ as x -> failwithf "more than 1 attr %A" x
        
// need to differentiate between computed and identity somehow
let hasIdentity fields =
    fields
    |> Seq.exists(getFieldAttr >> function |Computed,_ -> true | _ -> false)
    
type FieldMeta = {IsIdentity:bool;FieldInfo:FieldInfo}
type TypeMeta = {TableName:string;HasIdentity:bool; ToGen: FieldMeta list}

// exclude Computed
let getMeta (x:'T) : TypeMeta =
    match box x with 
    | :? IEnumerable -> failwithf "Get meta expects a single value"
    | _ -> ()
    let t = typeof<'T>
    let fields = typeof<'T>.GetFields()
    let fields = fields |> Seq.map(fun f -> f,getFieldAttr f) |> List.ofSeq
//    let hasIdentity = fields |> Seq.exists (function | _, (Identity,_) -> true | _ -> false)
    let fields = fields |> List.choose(function | f,(Identity,_) -> Some {IsIdentity=true;FieldInfo=f}; | _, (Computed,_) -> None | f,_ -> Some {IsIdentity=false;FieldInfo=f})
    let tableName = t.GetCustomAttributes() |> Seq.ofType<TableAttr> |> Seq.head |> fun x -> x.Name 
    {TableName=tableName; HasIdentity=fields |> Seq.exists(fun fm -> fm.IsIdentity);ToGen=fields}
    
let generateInsert identityIncluded meta (x:'T) =
    let nameValues = meta.ToGen |> List.map(fun f -> f.FieldInfo.Name, (f.FieldInfo.GetValue x |> generateColumnInsert))
    if debug then
        nameValues.Dump("name values")
    sprintf "insert into %s(%s) values (%s)" meta.TableName (nameValues |> Seq.map fst |> delimit ",") (nameValues |> Seq.map snd |> delimit ",")

let wrapIdentity tableName insertText =
    [
        sprintf "set identity_insert %s on" tableName
        insertText
        sprintf "set identity_insert %s off" tableName
    ] |> delimit "\r\n"

let generateInserts identityIncluded items =
    let meta = List.head items |> getMeta
    let inserts =  items |> List.map (generateInsert identityIncluded meta) |> delimit "\r\n"
    if identityIncluded then
        wrapIdentity meta.TableName inserts
    else inserts
    
    
match target with
| Single x ->
    let meta = getMeta x
    let f = if identityIncluded then wrapIdentity meta.TableName else id
    generateInsert identityIncluded meta target
    |> f
|Multiple items ->
    let meta = getMeta items.[0]
    generateInserts identityIncluded items
    
|> Dump
|> ignore

// 392 0 413 9/18/2018 10:10:36 AM 9/18/2018 10:10:36 AM 9/18/2018 12:00:00 AM False 99213| 125.00 null 
// select * from statements
