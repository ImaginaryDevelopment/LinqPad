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
    type XElement with
        static member GetElement1 name (x:XElement) = x.Element(XNamespace.None + name)
        static member GetElements1 name (x:XElement) = x.Elements() |> Seq.filter(fun x -> x.Name.LocalName = name)
        static member GetElements (x:XElement) = x.Elements()
    type String = 
        static member EqualsI (x:string) y =
            StringComparer.InvariantCultureIgnoreCase.Compare(x,y) = 0
        static member Delimit (delim:string) (x:string seq) = 
            String.Join(delim, Array.ofSeq x)
        
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
        let single<'T> x = 
            x
            |> Seq.truncate 2
            |> List.ofSeq
            |> function
                | (x:'T) :: [] -> x
                | [] -> raise <| InvalidOperationException "Single called with no elements"
                | _ -> raise <| InvalidOperationException "Single called with more than one element"
        let inline requires<'T> f (x:#seq<'T>) =
            if x :> seq<'T> |> f then
                x
            else raise <| InvalidOperationException "requires assertion not met"
            
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
type ConfigType = 
    | PackageRef of version:string
    | Config of newVersion:string*oldVersion:string
    with 
        member x.ToDump() =
            sprintf "%A" x

type Config = {Version:ConfigType; Name:string; Src:string; Raw:string}

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
    

    let pOut = 
        pConfigs 
        |> Seq.map (fun x -> x |> after target |> before "packages.config" |> trim1 "\\", readPConfig x)
        |> Seq.map (fun (path, x) ->       
            x |> Seq.map(fun x ->
                let text = x |> string
                let pId = x |> getAttrValue "id" |> Option.getOrDefault String.Empty
                {Src=path; Name=pId; Version=getAttrValue "version" x |> Option.getOrDefault String.Empty |> PackageRef;Raw=text}
            )
        )
        |> Seq.concat
        |> Seq.trySortByDesc(fun x -> x.Version)
        |> Seq.groupBy(fun x -> x.Name)
        |> Seq.map listFrom2nd
        |> Seq.filter(fun (_,x) -> x.Length > 1 && x |> Seq.map (fun x -> x.Version) |> Seq.distinct |> Seq.length |> fun x -> x > 1)
        |> Seq.trySortBy (fun (x,_) -> x <> "FSharp.Core")
        
module AppWebConfigs =
    let getAppConfigs () = 
        slnDir
        |> findConfigUnder
        |> Seq.filter(fun x -> Path.GetFileName x |> String.EqualsI "app.config" || Path.GetFileName x |> String.EqualsI "web.config")
        |> List.ofSeq
    let configs = getAppConfigs()
    let readConfig path = 
        XDocument.Load(uri=path)
        |> fun x -> x.Root.XPathSelectElement "runtime" 
        |> Option.ofObj
        |> Option.map (XElement.GetElements1 "assemblyBinding" >> Seq.collect (XElement.GetElements1 "dependentAssembly") >> List.ofSeq)

    let awOut =
        configs
        // eliminate items without binding redirects
        |> Seq.choose(fun path -> 
            match readConfig path with
            | Some [] -> None
            | Some x -> Some(path,x)
            | None -> None
        )
        |> Seq.collect (fun (path,x) -> 
            x
            |> Seq.map(fun x -> 
                try
                    let elements = x.Elements() |> Seq.map string |> List.ofSeq |> Seq.ofList
                    let br =  x |> XElement.GetElements1 "bindingRedirect" |> Seq.single
                    let nv = br |> getAttrValue "newVersion" |> Option.getOrDefault String.Empty 
                    let ov = br |> getAttrValue "oldVersion" |> Option.getOrDefault String.Empty 
                    let ct = ConfigType.Config(nv,ov)
                    {   Name= x |> XElement.GetElements1 "assemblyIdentity" |> Seq.single |> getAttrValue "name" |> Option.getOrDefault String.Empty
                        Version= ct
                        Raw= elements |> String.Delimit "\r\n"
                        Src=path |> after target |> before ".config" |> trim1 "\\"
                    }
                with ex ->
                    (path,x).Dump("failed")
                    reraise()
            )
        )
        |> Seq.groupBy(fun x -> x.Name)
        |> Seq.filter(fun (_, items) -> items |> Seq.map (fun x -> x.Version) |> Seq.distinct |> Seq.length |> fun x -> x > 1)

Util.HorizontalRun(false, PackageConfigs.pOut, AppWebConfigs.awOut)
|> Dump
|> ignore