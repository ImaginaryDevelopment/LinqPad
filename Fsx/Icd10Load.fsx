
#r "System.Xml"
#r "System.Xml.Linq.dll"
#r @"C:\TFS\Pm-Rewrite\Source-dev-rewrite\PracticeManagement\packages\FSharp.Data.2.2.5\lib\net40\FSharp.Data.dll"
open System
open System.Xml

let delimit delimiter (values:string seq) = String.Join(delimiter,values)
let replace (item:string) replace (s:string) = s.Replace(item,replace)
let wrap wrapper s = wrapper + s + wrapper
let wrap2 left right s = left + s + right
type Diagnosis = {Code:string;Desc:string}
type Column = {Name:string;IsMatch:bool}
type TableSpecifier = {TableName:string; HasIdentity:bool; Columns: Column seq; Values: string seq seq;}

open FSharp.Data
[<Literal>]
let drugPath = @"C:\TFS\ICD10CM_FY2015_Full_XML\FY15_Tabular.xml"

type Icd10 = XmlProvider< drugPath, Global=true >
let rec recurseDiag (diag : Icd10.Diag) =
    seq{
            yield diag;
            if diag.Diags <> null then
                for d in diag.Diags do 
                    yield! recurseDiag d
            else
                printfn "null diags found"
    }
let drugs = Icd10.Load(drugPath)

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
let sqlTemplate chunkValuesThreshold identifier hasIdentity tableSpec  :string= 
    let spacing = "    "
    let identity =
        let on = sprintf "SET IDENTITY_INSERT %s ON;" tableSpec.TableName
        let off = sprintf "SET IDENTITY_INSERT %s OFF;" tableSpec.TableName
        if hasIdentity then
            on,off
        else
            "",""
    let rowCount = tableSpec.Values |> Seq.length
    let values = tableSpec.Values |> Seq.map (delimit ",")
    let values = values |> Seq.map (fun v -> sprintf "%s%s%s(%s)" spacing spacing spacing v )
    let values = values |> Seq.chunkBySize chunkValuesThreshold |> Seq.map (delimit ("," + Environment.NewLine))
    let columns = tableSpec.Columns |> Seq.map (fun c -> "[" + c.Name + "]") 
    let columnList = columns |>  delimit (sprintf ",%s%s%s" Environment.NewLine spacing spacing)
    let onClause =tableSpec.Columns |> Seq.filter (fun c -> c.IsMatch) |> Seq.map (fun c -> sprintf "CTE.%s = TARGET.%s" c.Name c.Name) |> delimit " AND " // AND CTE.[Description] = TARGET.[Description]"

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
          VALUES(%s);
        """ columnList chunkValues columnList tableSpec.TableName onClause columnList columnList

    let chunks = 
        let length = Seq.length values
        values 
        |> Seq.mapi (fun i v-> 
            if length > 1 then
                sprintf "%s;%sprint 'finished chunk %i of %i';%sGO%s" (createMergeStatement v) Environment.NewLine (i+1) length Environment.NewLine Environment.NewLine
            else sprintf "%s;%s" (createMergeStatement v) Environment.NewLine)  
        |> delimit (Environment.NewLine + Environment.NewLine)

    sprintf """
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
PRINT 'Starting %s %s Synchronization for %i values'
GO
%s
-- chunks
%s
-- end chunks
%s
PRINT 'Done Synchronizing %s'
GO
    """ tableSpec.TableName identifier rowCount (fst identity) chunks (snd identity) tableSpec.TableName

let useDiagnosis values valueF = 
    let values = values |> Seq.map (fun v -> {v with Code = replace "." "" v.Code}) |> Seq.map (fun v -> [v.Code;"ICD10";v.Desc;v.Desc;v.Desc;sprintf "%s %s" v.Code v.Desc; ] |> Seq.ofList |> valueF) 
    let tbl = "[dbo].[Diagnoses]"
    let matchColumns = ["IcdCode";"CodeType"] |> List.map (fun c -> {Name=c;IsMatch=true})
    let columns = ["Diagnosis";"ShortDescription";"LongDescription";"FullTextSearch"] |> List.map (fun c -> {Name=c;IsMatch=false})
    let columns = matchColumns @ columns
    {Values = values; HasIdentity=true; TableName = tbl; Columns = columns }

let diagnosisTemplate identifier values =
    let valueF values' = values' |> Seq.map(replace "'" "''") |> Seq.map (wrap "'")
    let tableSpec =  useDiagnosis values valueF
    sqlTemplate 3000 identifier false tableSpec

let tryIt () = 
    let values = [
        {Code="A00"; Desc="Cholera"}
        {Code="A00.0";Desc="Cholera due to Vibrio cholerae 01, biovar cholerae"}
        ]
    diagnosisTemplate "tryIt" values

let getDiags () = 
    drugs.Chapters
    |> Seq.collect( fun l -> l.Sections)
    |> Seq.collect( fun s -> s.Diags)
    |> Seq.collect recurseDiag
    |> Seq.map (fun d -> 
        match d.Name.String with
        | Some n -> {Code=n; Desc=d.Desc}
        | _ -> failwithf "no name on diagnosis %A" d
        )

let sectionalizeDiags () = 
    drugs.Chapters
    |> Seq.map (fun c -> 
        match c.Name.String with 
        | Some name -> name, ( c.Sections
                            |> Seq.collect( fun s -> s.Diags)
                            |> Seq.collect recurseDiag
                            |> Seq.map (fun d -> 
                                match d.Name.String with
                                | Some n -> {Code=n; Desc=d.Desc}
                                | _ -> failwithf "no name on diagnosis %A" d
                                ))
        | _ -> failwithf "Chapter was unnamed"
        )

let scriptifySections () = 
    sectionalizeDiags()
    |> Seq.map (fun (name,diagnoses) -> sprintf "Icd10_Chapter%s_Inserts.sql" name, diagnosisTemplate (sprintf "Chapter%s" name) diagnoses)

let scriptTargetPath = @"C:\TFS\Pm-Rewrite\Source-dev-rewrite\PracticeManagement\ApplicationDatabase\Scripts\Post-Deployment\TableInserts\Icd10"
let writeDiagnosisScripts() = 
    scriptifySections()
    |> Seq.iter (fun (filename,text) ->
        let fullpath = System.IO.Path.Combine(scriptTargetPath, filename)
        System.IO.File.WriteAllText(fullpath,text)
    )

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
    getDiags()
    |> runInserts

let tryIt2 () = 
    getDiags() 
    |> Seq.take 2
    |> runInserts

open System.Diagnostics

let runProc filename args startDir = 
    let procStartInfo = 
        ProcessStartInfo(
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            FileName = filename,
            Arguments = args
        )
    match startDir with | Some d -> procStartInfo.WorkingDirectory <- d | _ -> ()

    let outputs = System.Collections.Generic.List<string>()
    let errors = System.Collections.Generic.List<string>()
    let outputHandler f (_sender:obj) (args:DataReceivedEventArgs) = f args.Data
    let p = new Process(StartInfo = procStartInfo)
    p.OutputDataReceived.AddHandler(DataReceivedEventHandler (outputHandler outputs.Add))
    p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (outputHandler errors.Add))
    let started = p.Start()
    if not started then
        failwithf "Failed to start process %s" filename
    p.BeginOutputReadLine()
    p.BeginErrorReadLine()
    p.WaitForExit()
    outputs,errors

let tf startDir args = 
    runProc @"C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\TF.exe" args startDir

let tfAdd basePath items =
    let addItem item = sprintf "add %s" item |> tf basePath
    let readOutputs (o,e) = 
        if e|> Seq.exists (fun e' -> String.IsNullOrWhiteSpace( e' ) = false) then
            o |> Seq.iter (printfn "%s")
            e |> Seq.iter (printfn "error:%s")
        else
            o |> Seq.iter (printfn "%s")
    items |> Seq.iter (addItem >> readOutputs)
