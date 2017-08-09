<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Core</NuGetReference>
</Query>

// find inconsistencies in app.config or packages.config for a solution

module Helpers =
    let after (delim:string) (x:string) = 
        let i = x.IndexOf(delim)
        x.Substring(i + delim.Length)
    let before (delim:string) (x:string) = 
        x.Substring(0, x.IndexOf(delim))
    let trim1 (delim:string) (x:string) = 
        x.Trim(delim |> Array.ofSeq)
    let listFrom2nd (x,y) = x, y |> List.ofSeq

    module Option = 
        let getOrDefault def =
            function
            | Some x -> x
            | None -> def
        let ofObj = 
            function
            | null -> None
            | x -> Some x
    let getAttrValue name (x:XElement) = x.Attribute(XNamespace.None + name) |> Option.ofObj |> Option.map (fun x -> x.Value)
    //let Dump x = x.Dump(); x
    module Seq = 
        // attempt to make it such that error'd sequences can still get dumped
        let trySortBy f x = 
            try
                x |> Seq.sortBy f
            with ex ->
                ex.Dump("sequence sort exception")
                x
        let trySortByDesc f x = 
            try
                x |> Seq.sortByDescending f
            with ex ->
                ex.Dump("sequence sort exception")
                x
            
open Helpers

let target = Environment.ExpandEnvironmentVariables("%devroot%")
let findUnder extension path = 
    Directory.EnumerateFiles(path, sprintf "*.%s" extension, SearchOption.AllDirectories)
let findSln = 
    findUnder "sln"
let findConfigUnder = 
    findUnder "config"
let slnDir = 
    findSln target
    |> List.ofSeq
    |> fun x -> x.[2]
    |> Path.GetDirectoryName

let getAppConfigs () = 
    slnDir
    |> findConfigUnder
    |> Seq.filter(fun x -> Path.GetFileName x = "app.config")
module PackageConfigs = 
    let getPackageConfigs () = 
        slnDir
        |> findConfigUnder
        |> Seq.filter(fun x -> Path.GetFileName x = "packages.config")
        |> List.ofSeq
    let pConfigs = getPackageConfigs()
    let readPConfig path = 
        XDocument.Load(uri=path)
        |> fun x -> x.XPathSelectElements "//package"
    // Package Config
    type PConfig = {Version:string; PName:string; Src:string; Raw:string} 
open PackageConfigs

let pOut = 
    pConfigs 
    |> Seq.map (fun x -> x |> after target |> before "packages.config" |> trim1 "\\", readPConfig x)
    |> Seq.map (fun (path, x) ->       
        x |> Seq.map(fun x ->
            let text = x |> string
            let pId = x |> getAttrValue "id" |> Option.getOrDefault String.Empty
            {Src=path; PName=pId; Version=getAttrValue "version" x |> Option.getOrDefault String.Empty;Raw=text}
        )
    )
    |> Seq.concat
    |> Seq.trySortByDesc(fun x -> x.Version)
    |> Seq.groupBy(fun x -> x.PName)
    |> Seq.map listFrom2nd
    |> Seq.filter(fun (_,x) -> x.Length > 1 && x |> Seq.map (fun x -> x.Version) |> Seq.distinct |> Seq.length |> fun x -> x > 1)
    |> Seq.trySortBy (fun (x,_) -> x <> "FSharp.Core")
Util.HorizontalRun(false, pOut, getAppConfigs())
|> Dump
|> ignore