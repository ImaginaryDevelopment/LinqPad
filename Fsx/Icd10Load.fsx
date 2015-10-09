
#r "System.Xml"
#r "System.Xml.Linq"
open System
open System.Xml
open System.Xml.Linq

module Helpers = 
    let delimit delimiter (values:string seq) = String.Join(delimiter,values)
    let replace (item:string) replace (s:string) = s.Replace(item,replace)
    let combine basePath path = System.IO.Path.Combine(basePath,path)
    let wrap wrapper s = wrapper + s + wrapper
    let wrap2 left right s = left + s + right
    let nullableToOpt x = if x = null then None else Some x
    let (|StartsWith|_|) delimiter (s:string) = if s.StartsWith(delimiter) then Some () else None
    let (|ExtensionNote|_|) text (s: string list) = if s |> Seq.length = 1 && s |> Seq.head |> (=) text then Some() else None
    let (|ExtensionNoteStartsWith|_|) text (s: string list) = if s |> Seq.length = 1 && s |> Seq.head |> (fun s-> s.StartsWith(text)) then Some() else None
    let (|AllowedExtensionNote|_|) rawCode (s:string list) =
        match s with
        | ExtensionNote (sprintf "The appropriate 7th character is to be added to all codes from category %s" rawCode)
        | ExtensionNote (sprintf "The appropriate 7th character is to be added to each code from category %s" rawCode) 
        | ExtensionNote (sprintf "The appropriate 7th character is to be added to each code from subcategory %s" rawCode) 
        | ExtensionNote (sprintf "One of the following 7th characters is to be assigned to each code in subcategory %s to designate the stage of glaucoma" rawCode) 
        | ExtensionNote (sprintf "The appropriate 7th character is to be added to each code from subcategory %s:" rawCode) 
        | ExtensionNote (sprintf "The appropriate 7th character is to be added to each code in subcategory %s" rawCode) 
        | ExtensionNote (sprintf "The appropriate 7th character is to be added to all codes in subcategory %s" rawCode) 
        | ExtensionNote (sprintf "The appropriate 7th character is to be added to each code in subcategory  %s" rawCode) // yes there is actually a code with two spaces as the only difference for the note
        | ExtensionNoteStartsWith (sprintf "One of the following 7th characters is to be assigned to each code under subcategory %s" rawCode) 
        | ExtensionNoteStartsWith (sprintf "The following appropriate 7th character is to be added to subcategory %s" rawCode)
            -> Some ()
        | _ -> None

open Helpers

module Seq = 
    let any (items:#seq<_>) = Seq.exists( fun _ -> true) items

open FSharp.Data
[<Literal>]
let drugPath = @"C:\TFS\ICD10CM_FY2015_Full_XML\FY15_Tabular.xml"

type Diagnosis = {Code:string;Desc:string;IsBillable:bool; Unextended:string}
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
let sqlTemplate chunkValuesThreshold identifier hasIdentity tableSpec  :int*string= 
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

    rowCount,(sprintf """
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
PRINT 'Starting %s %s'
PRINT 'Synchronization for %i values'
GO
%s
-- chunks
%s
-- end chunks
%s
PRINT 'Done Synchronizing %s'
GO
    """ tableSpec.TableName identifier rowCount (fst identity) chunks (snd identity) tableSpec.TableName)

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
        {Code="A00"; Desc="Cholera"; IsBillable=false;Unextended=null}
        {Code="A00.0";Desc="Cholera due to Vibrio cholerae 01, biovar cholerae";IsBillable=false;Unextended=null}
        ]
    diagnosisTemplate "tryIt" values

let getSectionalizedDiags() = 
    
    let xDoc = XDocument.Load(drugPath)
    let rootNs = xDoc.Root.Name.Namespace
    let getElementValue name (parent : XElement) = parent.Element(rootNs + name).Value |> nullableToOpt 
    let getElements name (parent:XElement) = parent.Elements(rootNs + name)
    let getDiagName = getElementValue "name"
    let getAttrValue name (parent:XElement) = 
        let attr = parent.Attribute(XNamespace.None + name)
        if attr = null then None else Some attr.Value

    let getAttrValueOrNull name parent = 
        match getAttrValue name parent with 
        | None -> null
        | Some x -> x

    rootNs,getElements "chapter" xDoc.Root
    |> Seq.map (fun c -> 
        let walkchapter node =c |> getElements "section" |> Seq.collect (getElements "diag")
        match getElementValue "name" c,getElementValue "desc" c with 
        | Some name, Some desc -> name,desc, walkchapter c
        | _ -> 
            let elementNames = c.Elements() |> Seq.map(fun e -> e.Name.LocalName) |> Seq.iter( printfn "unnamed chapter node: %s")
            failwithf "Chapter was unnamed or had no desc"
        )

let getIcd10Diags() = 
    let rootNs,diags = getSectionalizedDiags()
    let getElementValue name (parent : XElement) = parent.Element(rootNs + name).Value
    let getElements name (parent:XElement) = parent.Elements(rootNs + name)
    let getDiagName = getElementValue "name"
    let getAttrValue name (parent:XElement) = 
        let attr = parent.Attribute(XNamespace.None + name)
        if attr = null then null else attr.Value

    let rec descend parentExtensions node =
        let childDiagnoses = getElements "diag" node |> List.ofSeq

        let hasDiagChildren = childDiagnoses |> Seq.any
        let rawCode = node |> getDiagName
        let cleanedCode = rawCode |> replace "." String.Empty
        let baseDiagnosis = {Code= cleanedCode ;Desc= node|> getElementValue "desc";IsBillable = hasDiagChildren = false; Unextended=cleanedCode}
        //printfn "checking for extensions on node %A" baseDiagnosis
        let extensionElements = 
            let extensionElement = node.Element(rootNs + "sevenChrDef")
            if extensionElement = null then Seq.empty else extensionElement |> getElements "extension"
        let extensionNotes = 
            let extensionNotes = node.Element(rootNs + "sevenChrNote")
            if extensionNotes = null then Seq.empty else extensionNotes |> getElements "note" |> Seq.map (fun e -> e.Value)
            |> List.ofSeq
        let hasExtension = Seq.any extensionElements
        let descendChildren() = 
            if hasExtension then
                childDiagnoses |> Seq.map (descend (Some extensionElements)) |> Seq.collect id
            else 
                childDiagnoses |> Seq.map (descend None) |> Seq.collect id
        let walkWithExtensions extensions = 
            seq{
                        for x in extensionElements do
                            let value = x.Value
                            let attr = getAttrValue "char" x
                            yield { baseDiagnosis with Code= baseDiagnosis.Code.PadRight(6,'X') + attr;Desc = sprintf "%s (%s - %s)" baseDiagnosis.Desc attr value }
                    } |> List.ofSeq |> Seq.ofList
        //printfn "matching on node %A" baseDiagnosis
        let allowedExtensionCodes = ["M1A";"M80";"O31";"O32";"O35";"O36";"O40";"O41";"O64";"O69";"S00";"S01";"S02";"S42"]

        match parentExtensions,hasExtension,hasDiagChildren with
        | None, false, _ -> 
            seq {
                yield baseDiagnosis
                if hasDiagChildren then
                    yield! descendChildren()
            }
        | None,true,false -> walkWithExtensions extensionElements
        | Some parentExtensions, false, _ -> walkWithExtensions parentExtensions
        | None, true, true -> //decide what to do
            let limitMap = 
                [
                "S12",[0..6]
                "S49",[0..1]
                "S59",[0..2]
                "S79",[0..1]
                "S89",[0..3]
                ] |> Map.ofSeq
            if allowedExtensionCodes |> Seq.contains rawCode then 
                descendChildren()
            elif Map.containsKey rawCode limitMap then
                let useExtensions = limitMap.[rawCode] |> Seq.map (sprintf "%s.%i" rawCode) |> List.ofSeq
                childDiagnoses
                |> Seq.map ( fun c -> if useExtensions |> Seq.contains (getDiagName c) then descend (Some extensionElements) c else descend None c )
                |> Seq.collect id
            else
                match extensionNotes with
                | ExtensionNote (sprintf "The appropriate 7th character is to be added to all codes from category %s" rawCode)
                | ExtensionNote (sprintf "The appropriate 7th character is to be added to each code from category %s" rawCode) 
                | ExtensionNote (sprintf "The appropriate 7th character is to be added to each code from subcategory %s" rawCode) 
                | ExtensionNote (sprintf "One of the following 7th characters is to be assigned to each code in subcategory %s to designate the stage of glaucoma" rawCode) 
                | ExtensionNote (sprintf "The appropriate 7th character is to be added to each code from subcategory %s:" rawCode) 
                | ExtensionNote (sprintf "The appropriate 7th character is to be added to each code in subcategory %s" rawCode) 
                | ExtensionNote (sprintf "The appropriate 7th character is to be added to each code in subcategory  %s" rawCode) // yes there is actually a code with two spaces as the only difference for the note
                | ExtensionNoteStartsWith (sprintf "One of the following 7th characters is to be assigned to each code under subcategory %s" rawCode) 
                | ExtensionNoteStartsWith (sprintf "The following appropriate 7th character is to be added to subcategory %s" rawCode) -> descendChildren()
                | _ -> failwithf "Failed on node %s, hasDiagChildren: %A, hasExtension %A with note(s) %A" rawCode hasDiagChildren hasExtension extensionNotes
        //| Some _, true, _ -> failwithf "Failed on node %s, hasDiagChildren: %A,hasParentExtensions, hasExtension %A with note(s) %A" rawCode hasDiagChildren hasExtension extensionNotes 
        | Some parentExtensions, true, _ -> failwithf "Failed on node %s, hasDiagChildren: %A,hasParentExtensions, hasExtension %A with note(s) %A" rawCode hasDiagChildren hasExtension extensionNotes

    diags
    |> Seq.map (fun (sectionName,desc,diags) ->
        sectionName,desc,diags |> Seq.collect (descend None)
    )
    //|> Seq.map (fun d -> printfn "icd10Diag %s" (getElementValue "name" d); d)
    //|> Seq.filter(fun d -> d.Parent <> null && d.Parent.Name.LocalName="section")
    //|> Seq.map (descend None)
    //|> Seq.collect id

let scriptifySections () = 
    let diags = getIcd10Diags()
    diags 
    //|> Seq.map(fun (name,diags) -> name,Seq.length diags, diags)
    |> Seq.map (fun (name,desc, diagnoses) -> sprintf "Icd10_Chapter%s_Inserts.sql" name, diagnosisTemplate (sprintf "Chapter%s(%s)" name desc) diagnoses)

let scriptTargetPath = @"C:\TFS\Pm-Rewrite\Source-dev-rewrite\PracticeManagement\ApplicationDatabase\Scripts\Post-Deployment\TableInserts\Icd10"
let writeDiagnosisScripts beforeWriteF = 
    scriptifySections()
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
    getIcd10Diags()
    |> Seq.map (fun (_,_,diags) -> diags)
    |> Seq.collect id
    |> runInserts

let tryIt2 () = 
    getIcd10Diags()
    |> Seq.map (fun (_,_,diags) -> diags)
    |> Seq.collect id
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

let tfCheckout basePath items = 
    let checkout item = sprintf "checkout %s" item |> tf basePath
    let readOutputs (o,e) = 
        if e|> Seq.exists (fun e' -> String.IsNullOrWhiteSpace( e' ) = false) then
            o |> Seq.iter (printfn "%s")
            e |> Seq.iter (printfn "error:%s")
        else
            o |> Seq.iter (printfn "%s")
    items |> Seq.iter (checkout >> readOutputs)

let writeWithCheckout() = 
    writeDiagnosisScripts (Some (fun i -> tfCheckout None [i]))
