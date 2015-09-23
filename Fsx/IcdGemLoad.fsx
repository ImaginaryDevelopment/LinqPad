open System
let delimit delimiter (values:string seq) = String.Join(delimiter,values)
let replace (item:string) replace (s:string) = s.Replace(item,replace)
let wrap wrapper s = wrapper + s + wrapper
let wrap2 left right s = left + s + right
type Column = {Name:string;IsMatch:bool}
type TableSpecifier = {TableName:string; HasIdentity:bool; Columns: Column seq; Values: string seq seq;}
type IcdGem = {CodeFrom:string; CodeType:string; CodeTo:string; Flags:string}
#r @"C:\TFS\Pm-Rewrite\Source-dev-rewrite\PracticeManagement\packages\FSharp.Data.2.2.5\lib\net40\FSharp.Data.dll"
open FSharp.Data

[<Literal>]
let icd9Path = @"C:\Users\PROG7\Downloads\DiagnosisGEMs_2015\2015_I9gem.txt"
[<Literal>]
let icd10Path = @"C:\Users\PROG7\Downloads\DiagnosisGEMs_2015\2015_I10gem.txt"

let wordRegex = System.Text.RegularExpressions.Regex(@"(\w+)\s+(\w+)\s+(\w+)", System.Text.RegularExpressions.RegexOptions.Compiled)
let wordSplit s = 
    wordRegex.Match(s)
    |> (fun m -> m.Groups.[1].Value, m.Groups.[2].Value, m.Groups.[3].Value)

let extractCodes path = 
    System.IO.File.ReadAllLines(path) |> Seq.map wordSplit

open System.Windows.Forms
let toClipboard s = Clipboard.SetText(s)
#load "Icd10Load.fsx"
open Icd10Load

let useIcdGem values valueF = 
    let values = values |> Seq.map (fun v -> {v with CodeFrom = replace "." "" v.CodeFrom; CodeTo = replace "." "" v.CodeTo}) |> Seq.map (fun v -> [v.CodeFrom;v.CodeType;v.CodeTo;v.Flags] |> Seq.ofList |> valueF) 
    let tbl = "[dbo].[IcdGemMap]"
    let matchColumns = ["CodeFrom";"CodeType";"CodeTo"] |> List.map (fun c -> {Name=c;IsMatch=true})
    let columns = ["Flags"] |> List.map (fun c -> {Name=c;IsMatch=false})
    let columns = matchColumns @ columns
    {Values = values; HasIdentity=true; TableName = tbl; Columns = columns }

let icdGemTemplate identifier values =
    let valueF values' = values' |> Seq.map(replace "'" "''") |> Seq.map (wrap "'")
    let tableSpec =  useIcdGem values valueF
    sqlTemplate 3000 identifier false tableSpec

let tryIt () = 
    let values = [
        {CodeFrom="A00"; CodeTo="1123";Flags="0000"; CodeType="ICD10"}
        {CodeFrom="A00.0";CodeTo="1124";Flags="1000";CodeType="ICD10"}
        ]
    icdGemTemplate "tryIt" values

let mapInToGem codeType (s:string*string*string) =
    let codeFrom,codeTo,flags = s
    {CodeFrom=codeFrom;CodeTo=codeTo;Flags=flags;CodeType=codeType}

let getIcd9Gems() = 
    extractCodes icd9Path
    |> Seq.map (mapInToGem "ICD9")
let getIcd10Gems() = 
    extractCodes icd10Path
    |> Seq.map (mapInToGem "ICD10")

let scriptify () = 
    seq {
        yield! getIcd9Gems()
        yield! getIcd10Gems()
    }
    |> Seq.groupBy(fun c -> c.CodeType)
    |> Seq.map (fun (k,codes)-> sprintf "%sGem_Inserts.sql" k, icdGemTemplate k codes)

let writeGemScripts() = 
    scriptify()
    |> Seq.iter (fun (filename,text) ->
        let fullpath = System.IO.Path.Combine(scriptTargetPath, filename)
        System.IO.File.WriteAllText(fullpath,text)
    )
