<Query Kind="FSharpProgram" />

let devRoot = Environment.ExpandEnvironmentVariables("%devroot%")
let schemaPath = Path.Combine(devRoot, "Pm.Schema")
let toMakeLoaders = """
        module PyIMeta = Pm.Schema.DataModels.Accounts.PaymentItems.PaymentItemHelpers.Meta
    module AMeta = Pm.Schema.DataModels.Appointments.AppointmentHelpers.Meta
    module ChMeta = Pm.Schema.DataModels.Charges.ChargeHelpers.Meta
    module PIMeta = Pm.Schema.DataModels.PatientsInfoes.PatientsInfoHelpers.Meta"""
module Helpers = 
    let delimit d (x:string seq) = String.Join(d, x |> Array.ofSeq)
    let patternText = """let (|NullString|Empty|WhiteSpace|ValueString|) (s:string) =
        match s with
        | null -> NullString
        | "" -> Empty
        | _ when String.IsNullOrWhiteSpace s -> WhiteSpace
        | _ -> ValueString"""
open Helpers


toMakeLoaders.SplitLines()
|> Seq.filter(String.IsNullOrWhiteSpace >> not)
|> Seq.map (fun s -> s.Trim())
|> Seq.map (fun s -> 
    let r = Regex.Match(s,@"module (\w+)\s*=\s*Pm\.Schema\.DataModels\.((\w+)\.)?([\w]+)\.(\w+)Helpers\.Meta")
    let moduleAlias, schemaOpt, plural, name = r.Groups.[1].Value, r.Groups.[3].Value, r.Groups.[4].Value, r.Groups.[5].Value
    let filePath = 
        let makePath x= Path.Combine(schemaPath, sprintf "%s.generated.fs" x)
        let singular = makePath name
        let plural = makePath plural
        if File.Exists singular then singular else plural
    if File.Exists filePath |> not then failwithf "Could not locate file for '%s'" s
    let moduleText = 
        let lines = File.ReadAllLines filePath
        let moduleStartI = lines |> Seq.findIndex(fun x -> x.Trim().StartsWith("module Meta"))
        let indent = Regex.Match(lines.[moduleStartI],"\s+").Value
        lines.[moduleStartI + 1 ..]
        |> Seq.takeWhile( fun x -> x.StartsWith (sprintf "%s " indent))
        |> Seq.map(fun x -> x.Trim())
        // trim comments for loader
        |> Seq.filter(fun x -> x.Trim().StartsWith("let"))
        |> Seq.map (sprintf "  %s")
        |> List.ofSeq
        |> fun contents -> sprintf "module %s =" moduleAlias :: contents //@ [ ";;"]
        //|> fun contents -> lines.[moduleStartI]::contents
    
    moduleText |> delimit "\r\n"
    //moduleAlias, schemaOpt, plural, name, filePath, File.Exists filePath, s
)
|> List.ofSeq
|> fun x -> 
    [ 
        "open System"
        "let delimit d (x:string seq) = String.Join(d, x |> Array.ofSeq)"
        """let replace (target:string) (replacement) (str:string) = if String.IsNullOrEmpty target then invalidOp "bad target" else str.Replace(target,replacement)"""
        patternText] @ x @ [";;"]
|> delimit "\r\n"
|> Dump
|> ignore