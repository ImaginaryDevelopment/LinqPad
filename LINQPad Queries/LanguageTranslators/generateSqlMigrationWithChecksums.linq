<Query Kind="FSharpProgram" />

//[<AutoOpen>]
let before (delimiter:string) (x:string)  = x.Substring(0, x.IndexOf delimiter)
let after (delimiter:string) (x:string) = x.Substring(x.IndexOf(delimiter) + delimiter.Length)
let replace (target:string) (r:string) (x:string) = if String.IsNullOrEmpty target then invalidOp "bad target" else x.Replace(target,r)
let inline delimit delimiter (values:#seq<string>) = String.Join(delimiter, Array.ofSeq values)
let dumpt (t:string) x = x.Dump(t); x

type Railway<'t,'tFail> = 
    | Success of 't
    | Failure of 'tFail
    
module Option =
    let f fOpt x = 
        match fOpt with
        | Some f -> f x
        | None -> x

module Sql = 
    type ColumnInfo = {Name:string; Type:string; IsPK: bool}
    type TableInfo = {Schema:string; Name:string; Columns : ColumnInfo list}
    let mapColumnToTableAlias alias source = sprintf "%s.%s" alias source
    let print x = sprintf "print '%s'" x
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

type TargetColumn = string
type SourceColumn = string
type Aliaser = (string -> string)
type ColumnExpressionMapping = 
    | NoAliasing
    | UsesSource of (SourceColumn -> string)
    | UsesMultiple of (Aliaser -> string)
    
type ColumnExpressionSource = 
    | Constant of string * TargetColumn
    | Unchanged of SourceColumn // source and target are the same
    | Becomes of SourceColumn * TargetColumn
    | Mapped of (SourceColumn option)*TargetColumn * ColumnExpressionMapping
    
type Mapping = {Source:string;Target:string; SourceExpressionOpt: ColumnExpressionMapping option} with
    member x.ComposeSourceExpression aliaser = 
        match x.SourceExpressionOpt with
        | Some (UsesSource f)-> aliaser x.Source |> f
        | Some (UsesMultiple f) -> f aliaser
        | Some (NoAliasing) -> x.Source
        | None -> aliaser x.Source
        
let generateInsertMap sourceTable targetTable whereFOpt (map : Mapping seq) : string =
    let tc = map |> Seq.map (fun m -> m.Target) |> delimit ",\r\n        "

    let sc = map |> Seq.map (fun m -> m.ComposeSourceExpression id) |> delimit ",\r\n        "
    let printStart = sprintf "migrating %s to %s" sourceTable targetTable |> Sql.print 
    let where = match whereFOpt with | Some f -> sprintf "where %s" (f "s.") | None -> String.Empty
    let printEnd = sprintf "finished migrating %s to %s" sourceTable targetTable |> Sql.print
    sprintf """if not exists (select 1 from %s)
begin
    %s
    SET IDENTITY_INSERT %s ON
    insert into %s(
        %s) 
        
     select 
        %s
     from %s s
     %s
     ;
    %s
    SET IDENTITY_INSERT %s OFF
end
     """ targetTable printStart targetTable targetTable tc sc sourceTable where printEnd targetTable

let inline validateAllOrIncremental (title:string) count (fNeedsLimit:int option -> Railway<string,string * ^t >) fColumn =
    try
        match fNeedsLimit None with
        | Success query -> Util.OnDemand("ChecksumQuery", fun () -> query).Dump(); printfn "No limit returned no errors"
        //| None -> printfn "No Limit returned None, all column hashes match up"
        | Failure (query,_failrows) -> // some are invalid, lets try column by column
            let badGuy = 
                [1..count] 
                |> Seq.map (fun i -> i, Some i)
                |> Seq.map (fun (i,l) -> i, fNeedsLimit l) // (^a: (member Column1: _) str)
                //|> dumpt "incremental searched"
                |> Seq.choose(fun (i, badOpt) -> match badOpt with |Failure (query, fail) -> Some(i,query,fail) | Success _ -> None)
                |> Seq.filter (fun (_,_,failRow) -> (^t: (member Diff: int) failRow) <> 0)
                |> Seq.head
            let badGuy =
                let i,query,checkSums = badGuy
                try
                    let result = fColumn i
                    i, result, checkSums
                with ex ->
                    printfn "%i checksums were attempted" i
                    ex.Data.Add(sprintf "%s.i" title, i)
                    reraise()
            
            badGuy.Dump("bad guy!")
            
            ()
            
    with ex -> 
        ex.Data.Add(title,count)
        reraise()
        
    
let generateCheckSum sourceTable targetTable includeLastColumn transformColumnsOpt (fJoinClause) (wOpt:(string -> string) option) (map:Mapping seq)  : string = 
    let sourceColumns = 
        let fSourceAlias = Sql.mapColumnToTableAlias "s"
        map 
        |> Option.f transformColumnsOpt 
        |> Seq.map (fun m -> m.ComposeSourceExpression fSourceAlias) 
        
    let targetColumns = map |> Option.f transformColumnsOpt|> Seq.map (fun s -> Sql.mapColumnToTableAlias "t" s.Target)
    let sc = sourceColumns  |> delimit ","
    let tc = targetColumns  |> delimit ","
    let where = match wOpt with |None -> String.Empty | Some f-> (f "s." |> sprintf "where %s")
    let getPk schema table = 
        Sql.getPk schema table
        |> fun q -> dc.ExecuteQuery<string> q
    let colSelect =
        if includeLastColumn then 
            let sPk,tPk = getPk (sourceTable |> before ".") (sourceTable |> after ".") |> Seq.head, getPk (targetTable |> before ".") (sourceTable |> after ".") |> Seq.head
            // use targetPKey column Index in map to get sourceColumn transformed
            //(sPk,tPk,targetColumns).Dump("keys")
            let tPk = targetColumns |> Seq.mapi(fun i c -> i,c) |> Seq.find(fun (i,c) -> Sql.mapColumnToTableAlias "t" tPk = c)
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
        on %s -- s.chargeid = t.chargeid
    %s
) c
where c.sc - c.tc != 0"""  sc tc colSelect sourceTable targetTable (fJoinClause "s" "t") where

let becomes tc sc = Becomes(sc,tc)
let mapped tc f sc = Mapped(Some sc, tc, f)
let rawMap = [ // source, target
            "dbo.Charges","dbo.Charge", (sprintf "%s.ChargeId = %s.ChargeId"), Some (sprintf "%sPatientID is not null"),  [
                                        "ChargeID" |> Unchanged
                                        "CodeID" |> becomes "ProcedureCodeID"
                                        "Charge" |> mapped "Amount" (UsesSource (sprintf "coalesce(%s,0)")) // (fun fAlias -> fAlias >> sprintf "coalesce(%s,0)"))
                                        "PatientID" |> Unchanged
                                        "AppointmentID" |> Unchanged
                                        "ProviderId" |> becomes "RenderingProviderID"
                                        "Units" |> Unchanged
                                        "ControlNumber" |> Unchanged
                                        "TotalCharge" |> becomes "TotalAmount"
                                        "EmergencySvc" |> Unchanged
                                        "EPSPT" |> Unchanged
                                        "CopayException" |> Unchanged
                                        "PriorAuth" |> Unchanged
                                        "Referral" |> Unchanged
                                        "NoteToPayer" |> Unchanged
                                        "NDC" |> Unchanged
                                        "ImmunizationBatchNumber" |> Unchanged
                                        "NDCQuan" |> Unchanged
                                        "NDCUnit" |> Unchanged
                                        "SupervisingProviderID" |> Unchanged
                                        "CodeUnique" |> becomes "Modifier1"
                                        "Modifier2" |> Unchanged
                                        "Modifier3" |> Unchanged
                                        "Modifier4" |> Unchanged
                                        "Dia1" |> Unchanged
                                        "Dia2" |> Unchanged
                                        "Dia3" |> Unchanged
                                        "Dia4" |> Unchanged
                                        "ChargeDateTime" |> becomes "Inserted" ]
                                        
            "dbo.Payments","dbo.Payment", (sprintf "%s.PaymentId = %s.PaymentId"), 
                Some (fun s-> sprintf "%sPaymentType <> 4 and %sPaymentType <> 6 and %sPatientID is not null and exists(select 1 from patients p where p.patientid = %spatientid)" s s s s), [
                                        "PaymentID" |> Unchanged
                                        "PaymentType" |> mapped "PaymentTypeId" (UsesSource (sprintf "case %s when 5 then 'Era' else 'Patient' end"))
                                        // Schema.fs PaymentMethod.GetByValue line 217
                                        "PaymentType" |> mapped "PaymentMethodId" (UsesSource(sprintf "case %s when 1 then 'Cash' when 2 then 'CC' when 3 then 'Check' when 5 then 'Check' when 4 then '' when 7 then 'Other' when 8 then 'Fsa' end"))
                                        Constant ("'Complete'", "PaymentStatusId")
                                        "Amount"|> becomes "TotalAmount"
                                        "UserID" |> Unchanged
                                        Constant ("null","PayerID")
                                        "PatientID" |> Unchanged
                                        Constant ("null","PaymentTierId")
                                        "TimeStamp"|> becomes "Created"
                                        "CheckNumber" |> mapped "TransactionNumber" (UsesMultiple
                                            (fun fAlias  -> 
                                                let checkNumberColumn = fAlias "CheckNumber"
                                                let transColumn = fAlias "TransactioNumber"
                                                sprintf "case when %s is null or %s = '' then %s else %s end" checkNumberColumn checkNumberColumn transColumn checkNumberColumn
                                                ))
                                        "PaymentDate" |> becomes "Rcd"
                                        Constant ("cast (0 as bit)", "IsElectronic")
                                        // CCItemID
                                        "Comments" |> Unchanged
                                        ]
//            "dbo.Payments", "dbo.CCItem", [
//                                        "",""
//                                        ]
//            "dbo.Payments", "Payments.PaymentItem", [
//                                        "InsurancePayment", "Amount"]
            ]

        
let maps = 
    rawMap
    |> Seq.map (fun (s,t,jc,wOpt, m) -> 
        s,t,jc,wOpt, m |> Seq.map (fun (mt) -> 
                match mt with 
                |Unchanged s    ->      {Source = s; Target = s; SourceExpressionOpt = None}
                |Becomes (s,t)  ->      {Source = s; Target = t; SourceExpressionOpt = None}
                |Mapped (sOpt,t,f)   -> {Source=(match sOpt with |None -> null |Some s -> s); Target=t; SourceExpressionOpt = Some f }
                | Constant(s,t) ->      {Source=s; Target=t; SourceExpressionOpt = Some NoAliasing}
                //|MultiMapBecomes(t,f)   ->      {Source = s; Target = t; SourceMapOpt = Some (MultipleSources f)}
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
        

//let checkSumTake5Columns = map |> generateCheckSum (Some (fun items -> items |> Seq.take(5)))
maps 
|> Seq.iter (fun (s,t,jc,wOpt,maps) ->
    
        generateInsertMap s t wOpt maps
        |> dumpt "insert"
        let maps = maps |> Seq.filter(fun m -> match m.SourceExpressionOpt with | Some NoAliasing -> false | _ -> true)
        let fNeedsLimit cOpt : Railway<string, string * _ > = 
            let text = generateCheckSum s t (Option.isSome cOpt) (match cOpt with |Some c -> Some (fun columns -> columns |> Seq.take c) | None -> None) jc wOpt maps
            try
                let rowOpt = 
                    text
                    //|> dumpt "checksum query"
                    |> fun t -> dc.ExecuteQuery<CheckSumRow>(t) 
                    |> Seq.cast<CheckSumRow> 
                    |> Seq.tryHead
                match rowOpt with
                | Some badRow -> Failure (text,badRow)
                | None -> Success text
            with ex -> 
                ex.Data.Add("m.Length", maps |> Seq.length)
                ex.Data.Add("text",text)
                reraise()
        printfn "Validating %i Checksums" (maps |> Seq.length)
        validateAllOrIncremental (sprintf "%s to %s" s t) (maps |> Seq.length) fNeedsLimit (fun i -> List.ofSeq maps |> fun x -> x.[i - 1])
        printfn "Validated Checksums for %s" t
    )