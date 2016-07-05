<Query Kind="FSharpProgram" />

let before (delimiter:string) (x:string)  = x.Substring(0, x.IndexOf delimiter)
let after (delimiter:string) (x:string) = x.Substring(x.IndexOf(delimiter) + delimiter.Length)
let dumpt (t:string) x = x.Dump(t); x

module Option =
    let f fOpt x = 
        match fOpt with
        | Some f -> f x
        | None -> x
        
module Sql = 
    type ColumnInfo = {Name:string; Type:string; IsPK: bool}
    type TableInfo = {Schema:string; Name:string; Columns : ColumnInfo list}
    let getColumnInfo schema table = 
        sprintf """SELECT column_name as primarykeycolumn
                        --,object_id(ku.table_schema + '.' + ku.table_name) as tableObjId
                    ,t.name as TypeName
                FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS AS TC
                left JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS KU
                    ON  TC.CONSTRAINT_TYPE = 'PRIMARY KEY' AND
                        TC.CONSTRAINT_NAME = KU.CONSTRAINT_NAME
                left join sys.tables tbl 
                    on tbl.object_id = object_id(ku.table_schema + '.' + ku.table_name)
                left join sys.columns c 
                    on tbl.object_id = c.object_id 
                        and ku.column_name = c.name
                left join sys.types t
                    on c.system_type_id = t.system_type_id
                where ku.table_schema='%s' and ku.table_name = '%s'
 ORDER BY KU.ORDINAL_POSITION""" schema table
    
    let getPk schema table = // built from http://stackoverflow.com/questions/3930338/sql-server-get-table-primary-key-using-sql-query
        sprintf """SELECT column_name as primarykeycolumn 
--,KU.table_name as tablename, ku.table_schema
FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS AS TC
    JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS KU
        ON TC.CONSTRAINT_TYPE = 'PRIMARY KEY' AND
            TC.CONSTRAINT_NAME = KU.CONSTRAINT_NAME
            and ku.table_schema='%s'
            and ku.table_name='%s'
-- ORDER BY --KU.TABLE_NAME, KU.ORDINAL_POSITION
        """ schema table
    
    

type Mapping = {Source:string;Target:string;SourceMapOpt: (string -> string) option}
let inline delimit delimiter (values:#seq<string>) = String.Join(delimiter, Array.ofSeq values)

let generateInsertMap sourceTable targetTable (map : Mapping seq) =
    let tc = map |> Seq.map (fun m -> m.Target) |> delimit ",\r\n        "
    let sc = map |> Seq.map (fun m -> m.Source |> Option.f m.SourceMapOpt) |> delimit ",\r\n        "
    sprintf """insert into %s(
        %s) 
        
     select 
        %s
     from %s chOld""" targetTable tc sc sourceTable

let generateCheckSum sourceTable targetTable includeLastColumn transformColumnsOpt (map:Mapping seq)  : string = 
    let mapColumnToTableAlias alias source = sprintf "%s.%s" alias source
    let sourceColumns = map |> Option.f transformColumnsOpt |> Seq.map (fun s -> mapColumnToTableAlias "s" s.Source |> Option.f s.SourceMapOpt)
    let targetColumns = map |> Option.f transformColumnsOpt|> Seq.map (fun s -> mapColumnToTableAlias "t" s.Target)
    let sc = sourceColumns  |> delimit ","
    let tc = targetColumns  |> delimit ","
    let getPk schema table = 
        Sql.getPk schema table
        |> fun q -> dc.ExecuteQuery<string> q
    let colSelect =
        if includeLastColumn then 
            let sPk,tPk = getPk (sourceTable |> before ".") (sourceTable |> after ".") |> Seq.head, getPk (targetTable |> before ".") (sourceTable |> after ".") |> Seq.head
            // use targetPKey column Index in map to get sourceColumn transformed
            //(sPk,tPk,targetColumns).Dump("keys")
            let tPk = targetColumns |> Seq.mapi(fun i c -> i,c) |> Seq.find(fun (i,c) -> mapColumnToTableAlias "t" tPk = c)
            let sPk,tPk = sourceColumns |> List.ofSeq |> fun l -> l.[fst tPk], snd tPk
            let lastSc,lastTc = sourceColumns |> Seq.last, targetColumns |> Seq.last
            let addedSelect = sprintf ",%s as SId, %s as TId,%s as sProblem,%s as tProblem" sPk tPk lastSc lastTc
            //addedSelect.Dump("added select")
            addedSelect
        else String.Empty
    sprintf """
select c.*,
    c.sc - c.tc as diff
from (
    select  checksum(%s) as sc, 
              checksum(%s) as tc
              %s
from %s s -- source
 
join %s t -- target
    on s.chargeid = t.chargeid
) c
where c.sc - c.tc != 0"""  sc tc colSelect sourceTable targetTable 

let inline validateAllOrIncremental count (fNeedsLimit:int option -> ^t option) fColumn =
    match fNeedsLimit None with
    | None -> ()
    | Some _failure -> // some are invalid, lets try column by column
        let badGuy = 
            [1..count] 
            |> Seq.map (fun i -> i, Some i)
            |> Seq.map (fun (i,l) -> i, fNeedsLimit l) // (^a: (member Column1: _) str)
            //|> dumpt "incremental searched"
            |> Seq.choose(fun (i, badOpt) -> match badOpt with |Some bad -> Some(i,bad) | None -> None)
            |> Seq.filter (snd >> (fun x -> (^t: (member Diff: int) x) <> 0))
            |> Seq.head
            |> fun (i,checkSums) -> i,fColumn i, checkSums
        badGuy.Dump("bad guy!")
        ()
//        |> Seq.head
        
type MapType = 
    |Unchanged
    |Becomes of targetName:string
    |FTrans of (string -> string)
    |MapBecomes of targetName:string * sourceMapOpt:(string -> string)
    
let rawMap = [ // source, target
            "dbo.Charges","dbo.Charge", [
                                        "ChargeID", Unchanged 
                                        "CodeID", Becomes "ProcedureCodeID"
                                        "Charge", MapBecomes ("Amount", sprintf "coalesce(%s,0)")
                                        "PatientID", Unchanged
                                        "AppointmentID", Unchanged
                                        "ProviderId", Becomes "RenderingProviderID"
                                        "Units", Unchanged
                                        "ControlNumber", Unchanged
                                        "TotalCharge", Becomes "TotalAmount"
                                        "EmergencySvc", Unchanged
                                        "EPSPT", Unchanged
                                        "CopayException", Unchanged
                                        "PriorAuth", Unchanged
                                        "Referral", Unchanged
                                        "NoteToPayer", Unchanged
                                        "NDC", Unchanged
                                        "ImmunizationBatchNumber", Unchanged
                                        "NDCQuan", Unchanged
                                        "NDCUnit", Unchanged
                                        "SupervisingProviderID", Unchanged
                                        "CodeUnique", Becomes "Modifier1"
                                        "Modifier2", Unchanged
                                        "Modifier3", Unchanged
                                        "Modifier4", Unchanged
                                        "Dia1", Unchanged
                                        "Dia2", Unchanged
                                        "Dia3", Unchanged
                                        "Dia4", Unchanged
                                        "ChargeDateTime", Becomes "Inserted" ]
            "dbo.Payments","dbo.Payment", [
                                        "TimeStamp", Becomes "Created"
                                        "Amount", Becomes "TotalAmount"
                                        "PaymentType", Becomes "PaymentTypeId"
                                        "CheckNumber", Becomes "TransactionNumber"
                                        
            ]
            ]
let maps = 
    rawMap
    |> Seq.map (fun (s,t,m) -> 
        s,t,m |>Seq.map (fun (s,t) -> 
                match t with 
                |MapBecomes(t,f)    ->      {Source = s; Target = t; SourceMapOpt = Some f}
                |Unchanged          ->      {Source = s; Target = s; SourceMapOpt = None}
                |Becomes t          ->      {Source = s; Target = t; SourceMapOpt = None}
                |FTrans f           ->      {Source = s; Target = s; SourceMapOpt = Some f}
        )
    )

type CheckSumRow(diff) =
    let mutable diff = diff
    let mutable sProblem:obj = null
    let mutable tProblem:obj = null
    new() = CheckSumRow(-1)
    member x.Diff
        with get():int = diff
        and set v = diff <- v
    member x.SProblem
        with get() = sProblem
        and set v = sProblem <- v
    member x.TProblem
        with get() = tProblem
        and set v = tProblem <- v
    member val SId:int = 0 with get,set
    member val TId:int = 0 with get,set
        
        
    
//let executeCheckSumQuery t =


//let checkSumTake5Columns = map |> generateCheckSum (Some (fun items -> items |> Seq.take(5)))
maps 
|> Seq.iter (fun (s,t,m) ->
    
        generateInsertMap s t m
        |> dumpt "insert"
        let fNeedsLimit cOpt = 
            let text = generateCheckSum s t (Option.isSome cOpt) (match cOpt with |Some c -> Some (fun columns -> columns |> Seq.take c) | None -> None) m
            try
                text
            //|> dumpt "checksum query"
                |> fun t -> dc.ExecuteQuery<CheckSumRow>(t) 
                |> Seq.cast<CheckSumRow> 
                |> Seq.tryHead
            with ex -> 
                ex.Data.Add("text",text)
                reraise()
            
        validateAllOrIncremental (m |> Seq.length) fNeedsLimit (fun i -> List.ofSeq m |> fun x -> x.[i])
//
//        generateCheckSum s t None m
//        |> dumpt "checksum"
//        |> fun t -> dc.ExecuteQueryDynamic(t)
//        |> dumpt "checksum failures"
//        |> ignore
    )