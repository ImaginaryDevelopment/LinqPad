<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Core</NuGetReference>
</Query>

// replace all references to X with a consistent one (auto adjust for project depths differing?)
// also add to packages.config if not there
// project file with no package references won't have the content include entry =(
let targetRef = "FSharp.Core"
let desiredRefText = """<Reference Include="FSharp.Core, Version=4.4.1.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a">
          <AssemblyName>FSharp.Core.dll</AssemblyName>
          <HintPath>..\packages\FSharp.Core.4.2.2\lib\net45\FSharp.Core.dll</HintPath>
        </Reference>"""
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
    let replace (d:string) (r:string) (x:string) = 
        x.Replace(d,r)
    let mustReplace (d:string) r (x:string) =
        if x.IndexOf d < 0 then
            failwithf "Must replace did not find '%s'" d
        replace d r x
        
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

      
module ReverseDumper2 = 
    // reverse dumper with raw html support
    // linqpad helper (for when you can't .Dump(description), but still want the html)
    // also of note: there is an extra open div tag, because the ToHtmlString closes one that it didn't open
    let titleize t (x:obj) = 
        let objHtml = Util.ToHtmlString(enableExpansions=true, noHeader=true, objectsToDump= ([ x ] |> Array.ofList))
        let result = sprintf """<table class="headingpresenter">
        <tr>
        <th class="">%s</th>
        </tr>
        <tr>
        <td class="headingpresenter">%s</td>
        </tr>
        </table>"""                 t objHtml
        Util.RawHtml result
    type DumpType = 
        | Raw of string
        | DumpObj of obj
    // consider taking in obj, downcast to string, check for "<" starting the string to autodetect a raw html string? nah.    
    let dumpReverse :  DumpType -> unit =
        let dc = DumpContainer()
        dc.Dump() |> ignore
        (fun o -> 
            let o = 
                match o with
                | Raw s -> Util.RawHtml s
                | DumpObj o -> o
                
            match dc.Content with
            | :? (obj list) as items -> List.Cons(o,items)
            | _ -> [ o ]
            |> fun content -> dc.Content <- content
        )
    let dumpReverseT<'T>(x:'T) = 
        DumpObj x |> dumpReverse
        x
    let dumptRev t x = 
        titleize t x |> DumpObj |> dumpReverse |> ignore
        x
    // override/overwrite existing dump methods, no direct dumpReverse calls required
    type System.Object with
        member x.Dump() = printfn "override hit!";  x |> DumpObj |> dumpReverse |> ignore
        member x.Dump description = printfn "override2 hit! %s" description; x |> titleize description |> DumpObj |> dumpReverse |> ignore
open ReverseDumper2

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
    |> dumpReverseT
    |> Path.GetDirectoryName
//slnDir.Dump("sln dir")
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
    // this limits the search to only items where the refs disagree. our package config fix, needs all items, not just disagreements
    //|> Seq.filter(fun (_, items) -> items |> Seq.map snd |> Seq.distinct |> Seq.length |> fun x -> x > 1)
    |> Seq.tryFind(fun (k,v) -> k = targetRef)
    |> Option.map snd
projs.Dump("projects with fsharp.core refs?")
module RefFixing = 
    
        
    let getRefText t = 
        let start = Regex.Match(t,sprintf "<Reference.*%s" targetRef).Index
        let length = 
            let r = Regex.Match(t.[start..],"</Reference>")
            r.Index + r.Length
        t.[start..start+length]    
    let getFailHint oldText replacementText = 
        
        (oldText,replacementText) |> dumptRev "full text" |> ignore
        (
            let refText = getRefText oldText
            let repText = getRefText replacementText
            (refText,repText, refText = repText )
            |> dumptRev "orig,repl"
            |> ignore
        )
    let removeNs = replace " xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\"" String.Empty
    let replaceVersion slnFolder desiredValue relativeProjPath currentValue = 
        (* TODO: depth comparisons ? so the relative paths work on non-homogenous projects *)
        // current value includes extra namespace literal which the original files don't have
        
        let fullProjectPath = Path.Combine(slnFolder, relativeProjPath)
        
        let text = File.ReadAllText fullProjectPath
        let currentValue = getRefText text
        let chk = text.GetHashCode()
        let text' = text |> mustReplace currentValue desiredValue
        let chk2 = text'.GetHashCode()
        if chk = chk2 then 
            getFailHint text text'
            failwith "replace did not update"
        File.WriteAllText(fullProjectPath,text')
        
    
    let fixRefs debug = 
        let dumpReverseT x = 
            if debug then 
                x |> dumpReverseT
            else x
        
        projs
        |> dumpReverseT
        
        |> Option.map (Seq.filter(fun (_,v) -> (v |> removeNs) <> desiredRefText) >> List.ofSeq)
        |> dumpReverseT
        |> Option.iter(fun toFix ->
            sprintf "fixing %i refs" toFix.Length |> dumpReverseT |> ignore
            toFix
            |> Seq.iter(fun (relPath,actualValue) ->
                replaceVersion slnDir desiredRefText relPath.[1..] actualValue
            )
        )
        |> ignore
// fix package.config files too
module PConfigFixing = 
    type ProjectConfigMeta = {RelativeProjPath:string; HasPackageConfigFile:bool;PackageConfigPath:string;FullPath:string}
        
    let fixProjectConfigs debug relPaths = 
        relPaths // relativeProjPaths
        |> Seq.map(fun (rpp:string) ->
            let fullProjectPath = Path.Combine(slnDir, rpp.[1..])
            if not <| File.Exists fullProjectPath then
                failwithf "Project file didn't exist at '%s'" fullProjectPath
            
            let pc = Path.Combine(Path.GetDirectoryName fullProjectPath, "packages.config")
            {RelativeProjPath=rpp; PackageConfigPath = pc; HasPackageConfigFile = File.Exists pc; FullPath = fullProjectPath }
        )
        |> dumptRev "meta"
//        |> Seq.choose(fun ->
//            let fullProjectPath = Path.Combine(slnDir, relativeProjPath)
//            let pc = Path.Combine(Path.GetDirectoryName fullProjectPath, "packages.config")
//            pc.Dump("package.config?")
//            if File.Exists pc then
//                Some pc
//            else None
//        )
//        |> dumptRev "package configs!"
        |> ignore
    
    ()
RefFixing.fixRefs false
projs
|> dumpReverseT
|> Option.iter (List.map fst >> PConfigFixing.fixProjectConfigs true)

