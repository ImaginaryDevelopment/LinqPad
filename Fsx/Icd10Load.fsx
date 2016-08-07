#load "Icd10Reader.fsx"
open System

[<Literal>]
let drugPath = @"C:\TFS\ICD10CM_FY2015_Full_XML\FY15_Tabular.xml"

open Icd10Reader
open Icd10Reader.Helpers

type Column = {Name:string;IsMatch:bool}
type TableSpecifier = {TableName:string; HasIdentity:bool; Columns: Column seq; Values: string seq seq;}

//goal: be completely ignorant of type
// statement * ((parameter * value) seq )) seq
let sqlRowTemplate tableSpec :(string * (string * string) seq) seq =
    let columnParams = tableSpec.Columns|> Seq.map (fun c -> "@" + c.Name)
    let columns = tableSpec.Columns |> Seq.map (fun c -> "[" + c.Name + "]")
    let columnList = columns |>  delimit ","
    let where = 
        columnParams 
        |> Seq.zip tableSpec.Columns
        |> Seq.filter(fun (c,_)-> c.IsMatch)
        |> Seq.map (fun (c,p) -> sprintf "%s = %s" c.Name p)
        |> delimit " AND "

    let insertStatement = sprintf "IF NOT EXISTS(SELECT 1 FROM %s WHERE %s) INSERT %s (%s) VALUES (%s)" tableSpec.TableName where tableSpec.TableName columnList (delimit ","columnParams)

    seq{
        if tableSpec.HasIdentity then
            yield sprintf "SET IDENTITY_INSERT %s ON;" tableSpec.TableName, Seq.empty
        printfn "columnParams %A" columnParams
        printfn "values %A" tableSpec.Values
        for row in tableSpec.Values do
            yield insertStatement, Seq.zip columnParams row

        if tableSpec.HasIdentity then
            yield sprintf "SET IDENTITY_INSERT %s OFF;" tableSpec.TableName, Seq.empty
    }

//goal: be completely ignorant of type
let getSqlTemplate hashOpt chunkValuesThreshold identifier hasIdentity tableSpec  :int*string= 
    let spacing = "    "
    let ifHash = // no GO statements inside if-begin
        match hashOpt with
        | Some x -> 
            let startIf = sprintf @"print 'checking diagnoses for icd10 hash'
if (select checksum_agg(checksum(*)) from %s where CodeType = 'ICD10') <> %i -- hash idea from http://stackoverflow.com/a/1560450/57883
begin
"                           tableSpec.TableName x
            let endIf = @"end
else
begin
    print 'skipping icd10 inserts, hash matches'
end"
            startIf,endIf
        | None -> String.Empty,String.Empty
    let identity =
        if hasIdentity then
            let on = sprintf "SET IDENTITY_INSERT %s ON;" tableSpec.TableName
            let off = sprintf "SET IDENTITY_INSERT %s OFF;" tableSpec.TableName
            on,off
        else
            "",""
    let rowCount = tableSpec.Values |> Seq.length
    let values = tableSpec.Values |> Seq.map (delimit ",")
    let values = values |> Seq.map (fun v -> sprintf "%s%s%s(%s)" spacing spacing spacing v )
    let values = values |> Seq.chunkBySize chunkValuesThreshold |> Seq.map (delimit ("," + Environment.NewLine))
    let columns = tableSpec.Columns |> Seq.map (fun c -> "[" + c.Name + "]") 
    let columnList = columns |>  delimit (sprintf ",%s%s%s" Environment.NewLine spacing spacing)
    let onClause = tableSpec.Columns |> Seq.filter (fun c -> c.IsMatch) |> Seq.map (fun c -> sprintf "CTE.%s = TARGET.%s" c.Name c.Name) |> delimit " AND " // AND CTE.[Description] = TARGET.[Description]"

    let createMergeStatement chunkValues = 
        sprintf """
        WITH CTE AS
        (
           SELECT %s
           FROM (VALUES 
        %s
           )
              AS SOURCE(%s)
        )
        MERGE INTO %s AS TARGET
        USING CTE
        ON %s
        WHEN NOT MATCHED BY TARGET THEN
          INSERT(%s)
          VALUES(%s)
        WHEN MATCHED AND CTE.ISBILLABLE <> TARGET.ISBILLABLE THEN
            UPDATE set target.isbillable=cte.isbillable ; """ columnList chunkValues columnList tableSpec.TableName onClause columnList columnList

    let chunks = 
        let length = Seq.length values
        values 
        |> Seq.mapi (fun i v-> 
            if length > 1 then
                sprintf "%s\r\n%sprint 'finished chunk %i of %i';%s" (createMergeStatement v) Environment.NewLine (i+1) length Environment.NewLine 
            else sprintf "%s;%s" (createMergeStatement v) Environment.NewLine)  
        |> delimit (Environment.NewLine + Environment.NewLine)

    rowCount,(sprintf """
SET ANSI_NULLS ON
SET QUOTED_IDENTIFIER ON
PRINT 'Starting %s %s'
PRINT 'Synchronization for %i values'
%s
%s
-- chunks
%s
-- end chunks
%s
%s
PRINT 'Done Synchronizing %s'
    """ tableSpec.TableName identifier rowCount (fst ifHash) (fst identity) chunks (snd identity) (snd ifHash) tableSpec.TableName)

let useDiagnosis values valueF = 
    let values = 
        values 
        |> Seq.map (fun v -> {v with Code = replace "." "" v.Code}) 
        |> Seq.map (fun v -> 
            [v.Code;"ICD10";v.Desc;v.Desc;v.Desc;sprintf "%s %s" v.Code v.Desc;sprintf "%b" v.IsBillable ] 
            |> Seq.ofList 
            |> valueF) 
    let tbl = "[dbo].[Diagnoses]"
    let matchColumns = ["IcdCode";"CodeType"] |> List.map (fun c -> {Name=c;IsMatch=true})
    let columns = ["Diagnosis";"ShortDescription";"LongDescription";"FullTextSearch";"IsBillable"] |> List.map (fun c -> {Name=c;IsMatch=false})
    let columns = matchColumns @ columns
    {Values = values; HasIdentity=true; TableName = tbl; Columns = columns }

let getDiagnosisTemplate hashOpt identifier values =
    let valueF values' = values' |> Seq.map(replace "'" "''") |> Seq.map (wrap "'")
    let tableSpec =  useDiagnosis values valueF
    getSqlTemplate hashOpt 3000 identifier false tableSpec

let tryIt () = 
    let values = [
        {Code="A00"; Desc="Cholera"; IsBillable=false;Unextended=null}
        {Code="A00.0";Desc="Cholera due to Vibrio cholerae 01, biovar cholerae";IsBillable=false;Unextended=null}
        ]
    getDiagnosisTemplate None "tryIt" values

let checkForMissingCodes (chapter,title,diagnoses:#seq<Diagnosis>) = 
    match chapter with
    | "19" -> // 15597 codes on last attempt
        ["S9301XA";"S9301XD";"S9302XA"]
        |> Seq.iter(fun code ->
        match diagnoses |> Seq.tryFind (fun d -> d.Code= code) with
        | Some x -> ()
        | None -> 
            
            printfn "Found %i diagnoses in chapter %s" (Seq.length diagnoses) chapter
            failwithf "Expected to find code %s in chapter %s under title %s" code chapter title
        )
        (chapter,title,diagnoses)
    | _ -> (chapter,title,diagnoses)

let scriptifySections interestedCodeBeginnings chapterFilter hashOpt = 
    let diags = Icd10Reader.Diags.getIcd10Diags drugPath interestedCodeBeginnings
    match chapterFilter with 
    |Some f -> diags |> Seq.filter f
    |None -> diags
    |> Seq.map checkForMissingCodes
    |> Seq.map (fun (name,desc, diagnoses) -> sprintf "Icd10_Chapter%s_Inserts.sql" name, getDiagnosisTemplate hashOpt (sprintf "Chapter%s(%s)" name desc) diagnoses)

let scriptTargetPath = @"C:\TFS\PracticeManagement\dev\PracticeManagement\Db\Scripts\Post-Deployment\TableInserts\Icd10"

let writeDiagnosisScripts hashOpt beforeWriteF interestedCodeBeginnings chapterFilter= 
    if not <| System.IO.Directory.Exists scriptTargetPath then
        failwithf "Script target directory did not exist: %s" scriptTargetPath
    scriptifySections interestedCodeBeginnings chapterFilter hashOpt
    |> Seq.map (fun (filename,(rowCount,text)) ->
        let fullpath = System.IO.Path.Combine(scriptTargetPath, filename)
        match beforeWriteF with
        |Some f -> f fullpath
        | _ -> ()
        System.IO.File.WriteAllText(fullpath,text)
        rowCount
    )
    |> Seq.sum
    |> printfn "Wrote %i diagnoses"

open System.Data.SqlClient
#r "System.Transactions"

let runInserts values= 
    let tableSpec = 
        useDiagnosis values id
    let tableSpec = {tableSpec with HasIdentity = false}
    let statements = sqlRowTemplate tableSpec
    use cn = new SqlConnection("Data Source=(local);Integrated Security=SSPI;Initial Catalog=PmRewriteApplicationDatabase;app=Icd10LoadScripts")
    cn.Open()
    use cmd = new SqlCommand()
    cmd.Connection <- cn
    for statement,params' in statements do
        cmd.CommandText <- statement
        cmd.Parameters.Clear()
//        if Seq.isEmpty params' then
//        else
        for p,v in params' do
            cmd.Parameters.AddWithValue(p,v) |> ignore
        cmd.CommandType <- System.Data.CommandType.Text
        try
            cmd.ExecuteNonQuery() |> ignore
        with |ex ->
            printfn "exception: %A" statement
            params' 
            //|> Seq.map snd
            |> printfn "params %A" 
            ex.Data.Add("values",sprintf "%A" params')
            ex.Data.Add("text",cmd.CommandText)
            printfn "%A" ex

let runInserts'() = 
    Icd10Reader.Diags.getIcd10Diags drugPath None
    |> Seq.map (fun (_,_,diags) -> diags)
    |> Seq.collect id
    |> runInserts

let tryIt2 () = 
    Icd10Reader.Diags.getIcd10Diags drugPath None
    |> Seq.map (fun (_,_,diags) -> diags)
    |> Seq.collect id
    |> Seq.take 2
    |> runInserts

// tf None "workspaces /collection:http://tfs20102.xpress.domain:8080/tfs";;
let writeWithCheckout() = 
    let interestedCodeBeginnings = (Some ["S548"])
    let diagFilter = None //(Some (fun (ch,_,_) -> ch="19"))
    writeDiagnosisScripts (Some 792943353) (Some (fun i -> tfCheckout (Some(scriptTargetPath)) [i])) interestedCodeBeginnings diagFilter

writeWithCheckout()