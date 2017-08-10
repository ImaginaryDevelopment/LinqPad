<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Core</NuGetReference>
</Query>

// find references in *.*proj files for comparison of paths and duplications

module Helpers =
    type String = 
        static member EqualsI (x:string) y =
            StringComparer.InvariantCultureIgnoreCase.Compare(x,y) = 0
        static member Delimit (delim:string) (x:string seq) = 
            String.Join(delim, Array.ofSeq x)
        static member Contains (delim:string) (x:string) = 
            x.Contains(value=delim)
    let after (delim:string) (x:string) = 
        let i = x.IndexOf(delim)
        x.Substring(i + delim.Length)
    let before (delim:string) (x:string) = 
        x.Substring(0, x.IndexOf(delim))
    let beforeOrSelf delim x = 
        if String.Contains delim x then
            x |> before delim
        else x
    let trim1 (delim:string) (x:string) = 
        x.Trim(delim |> Array.ofSeq)
    let listFrom2nd (x,y) = x, y |> List.ofSeq
    type XElement with
        static member GetElement1 name (x:XElement) = x.Element(XNamespace.None + name)
        static member GetElements1 name (x:XElement) = x.Elements() |> Seq.filter(fun x -> x.Name.LocalName = name)
        static member GetElements (x:XElement) = x.Elements()

            
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
        let mapToTuple f =
            Seq.map(fun x -> x, f x)
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
    module Tuple =
        let mapSnd f=
            Seq.map (fun (x,y) -> x, y |> f)
open Helpers

module Dumper = 
    let mutable private items = []
    let private dc = DumpContainer() |> Dump
    let dumpReverse<'T>(x:'T) = 
        items <- box x :: items
        dc.Content <- items
        x
open Dumper

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
type ItemReference = { Name:string; Raw:string }
let projs =
    Directory.EnumerateFiles(slnDir,"*.*proj", SearchOption.AllDirectories)
    |> Seq.choose (fun projPath ->
        try
            (projPath,XDocument.Load(projPath).Root)
            |> Some
        with _ ->
            None
    )
    |> Seq.map (fun (p,xe) -> p, xe |> XElement.GetElements1 "ItemGroup" |> Seq.collect (XElement.GetElements1 "Reference") |> List.ofSeq)
    |> Seq.map(fun (path,refs) ->
        path, refs |> Seq.map(fun r ->
            {Name=getAttrValue "Include" r |> Option.getOrDefault String.Empty |> beforeOrSelf ",";Raw= r |> string}    
        ) |> List.ofSeq
    )
    |> Seq.collect(fun (path,refList) -> 
        refList |> Seq.map(fun r ->
            path,r
        )
    )
    |> Seq.groupBy(fun (_,r) -> r.Name)
    |> Seq.map(fun (k, items) -> k, items |> Seq.map(fun (path,r) -> path |> after target,r.Raw) |> List.ofSeq)
    |> Seq.filter(fun (_, items) -> items |> Seq.map snd |> Seq.distinct |> Seq.length |> fun x -> x > 1)
    

projs
|> dumpReverse
|> ignore