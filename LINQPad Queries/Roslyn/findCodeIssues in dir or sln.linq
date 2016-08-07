<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE.dll</Reference>
  <NuGetReference>Microsoft.CodeAnalysis</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.CodeAnalysis</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp.Syntax</Namespace>
</Query>

// find code issues like methods that have a try but don't say so in the method name
// consider: also find swallows

//with help from https://github.com/dotnet/roslyn/wiki/Getting-Started-C%23-Semantic-Analysis
//translate to F# so that we can choose all files under directory, or all files in all projects in a solution

// if we decide to have the file/method auto open on click in the output this is the hook
// var devEnvs = System.Diagnostics.Process.GetProcessesByName("devenv"); //.Dump();

// this roslyn version is working as of 7/21/2016 with Microsoft.CodeAnalysis 1.3.2
// syntax reference site: http://source.roslyn.codeplex.com/#Microsoft.CodeAnalysis.CSharp/Syntax.xml.Generated.cs,4a09ef72a9269723
let debug = false

[<AutoOpen>]
module LinqPad =
    let dumpt (title:String) x = x.Dump(title); x
    let dumptif (title:String) b x = if b then dumpt title x else x
    let dumph x = Util.Highlight(x).Dump() |> ignore
    
[<AutoOpen>]
module LinqHelpers = 
    let ofType<'t> (x:IEnumerable<_>) = x.OfType<'t>()
    let any (x:IEnumerable<_>) = x.Any()
    let anyf f (x:IEnumerable<_>) = x.Any(f)
    
[<AutoOpen>]
module Functionalizers = 
    let tuplef f x= x,f x
    let tuplef1 f (a,x) = a,x,f x
    let after (delimiter:string) (s:string) = s.Substring(s.IndexOf delimiter + delimiter.Length)
    let before (delimiter:string) (s:string) = s.Substring(0,s.IndexOf delimiter)
    //let flip f y x = f x y
    let startsWith delimiter (s:string) = s.StartsWith delimiter
    let endsWith delimiter (s:string) = s.EndsWith delimiter
    let trimStart (s:string) = s.TrimStart()
    let tee f x = f x, x
    let combine x y = Path.Combine [| x;y |]
//    let isRegexMatch pattern input = Regex.IsMatch(input,pattern)
//    let regexMatch pattern input = Regex.Match(input,pattern).Groups.[1].Value
//    let regexMatch2 pattern input = 
//        Regex.Match(input,pattern)
//        |> (fun m -> m.Groups.[1].Value, m.Groups.[2].Value)
    let getDescendantNodes (sn :#SyntaxNode) = sn.DescendantNodes()
    
type Result<'a> =
    |Success of 'a
    |Failure of string
    with
        member x.Map success failure =
            match x with 
            | Success s -> success s
            | Failure error -> failure error
        static member bind f x = 
            match x with
            | Success x -> f x
            | Failure s -> Failure s // must reconstruct as 'a may be changing    
            
        static member choose f x = 
            x|> Seq.choose (function | Success x -> f x |> Some | Failure _ -> None)
            
type DirPath = 
    | DirPath of string
    with
        member x.Value = 
            match x with
            |DirPath s -> s
            
let DirPath path = 
    if Directory.Exists path then
        DirPath path |> Success
    else
        sprintf "Directory not found at %s" path |> Failure 

type FilePath = 
    |FilePath of string
    with
        member x.Value =
            match x with
            |FilePath s -> s
        member x.ReadAllLines () = 
            x.Value |> File.ReadAllLines
        override x.ToString () = 
            sprintf "%A" x
            
let FilePath path = 
    if File.Exists path then
        FilePath path |> Success
    else
        sprintf "File not found at %s" path |> Failure
        
//type ValidPath =
//    |DirPath
//    |FilePath
module BRoslyn = 
    let getTree (FilePath path):SyntaxTree = 
        File.ReadAllText path
        |> CSharpSyntaxTree.ParseText
        
    let getRoot tree = 
        let getRoot (tree:SyntaxTree) = tree.GetRoot()
        getRoot tree :?> CompilationUnitSyntax
    
    module Methods = 
        type private MDS = MethodDeclarationSyntax
        let getMethods (root:CompilationUnitSyntax) = 
            root
            |> getDescendantNodes
            |> ofType<MDS>
            |> Array.ofSeq
            
        let isOverride (mds:MDS) = mds.Modifiers.Any(fun m -> m.Text = "override")
        let hasTry (mds:MDS) = mds |> getDescendantNodes |> ofType<TryStatementSyntax> |> any
        
        let isTryMethodProperlyNamed (mds:MDS) = 
            mds.Identifier.Text.StartsWith("Try", StringComparison.CurrentCultureIgnoreCase)
            
// code related to reading the raw .sln file        
module Sln = 
    type SlnInfo = { SlnFolderGuid :Guid; Name:string; Path:FilePath; ProjectGuid:Guid}
    type SlnItem = 
        |SlnProject of FilePath
        |SlnFolder of DirPath
    let isCsProj (FilePath path)= endsWith ".csproj" path
    
    let readSln (FilePath slnPath) = 
        let (|Project|Folder|Error|) (DirPath slnDir, relPath) = 
            let combined = combine slnDir relPath
            if Seq.exists (fun ending -> endsWith ending relPath) [ ".proj";".csproj";".fsproj";".dbproj";".sqlproj" ] then
                match combined |> FilePath with
                | Success fp -> Project fp
                | Failure s -> Error s
            else
                match combined |> DirPath with
                | Success dp -> Folder dp
                | Failure s -> Error s
        
        let slnDir = Path.GetDirectoryName slnPath |> DirPath

        let afterQuote x = x |> after "\""
        let beforeQuote x = x |> before "\""
        let betweenQuotes x = x |> after "\"" |> before "\""
        
        let mapProjectLine (l:string) = 
            let l = l |> after "Project(\""
            //l.Dump("mapping/parsing")
            let slnFolderGuid, l = l |> beforeQuote |> Guid.Parse, l |> afterQuote 
            //(slnFolderGuid,l).Dump("mapping/parsing2")
            let name, l = l |> betweenQuotes, l |> afterQuote |> afterQuote
            let path, l = l |> betweenQuotes, l |> afterQuote |> afterQuote
            //(slnFolderGuid,name,path,l).Dump("mapping/parsing4")
            let itemGuid, l = l |> betweenQuotes |> Guid.Parse, l |> afterQuote |> afterQuote
            
            (slnFolderGuid, name, path, itemGuid)
            //|> dumpt "mapping/parsing end"
            
        let mapProjectLineItems (slnFolderGuid, name, path:string, itemGuid) = 
            let dirPath = Path.GetDirectoryName slnPath |> DirPath
            match dirPath with 
            |Success dp -> 
                match dp, path with
                |Project(fp:FilePath) -> (slnFolderGuid,name,SlnItem.SlnProject fp, itemGuid) |> Success
                //|_ -> Failure "unmatched case"
                |Folder(dp:DirPath) -> (slnFolderGuid,name,SlnItem.SlnFolder(dp),itemGuid) |> Success
                |Error(s:string) -> Failure(s)
            |Failure s-> Failure s
            
        let mapProjectItem = 
            function
            | (slnFolderGuid, name, SlnProject fp, itemGuid) -> Some {SlnFolderGuid=slnFolderGuid; Name=name; Path= fp; ProjectGuid = itemGuid}
            | (_, _, SlnFolder dp, _) -> None
        
        File.ReadAllLines slnPath
        |> Seq.skipWhile (trimStart >> startsWith "Project(\"{" >> not)
        |> Seq.takeWhile (startsWith "Global" >> not )
        |> Seq.filter (trimStart >> startsWith "Project(\"{")
        |> dumptif "raw sln lines" debug
        |> Seq.map mapProjectLine
        |> dumptif "readSln before validation" debug
        |> Seq.map mapProjectLineItems
        |> Result.choose mapProjectItem
        |> dumptif "readSln" debug

//        .ToDictionary(t => t.ProjectGuid)

let target= System.Environment.GetEnvironmentVariable("devroot") + @"\PracticeManagement\dev\PracticeManagement\"

let slnProjects : Sln.SlnInfo seq = 
    target + "PracticeManagementRW_local.sln"
    |> FilePath
    |> Dump
    |> Result.bind (Sln.readSln >> Success)
    |> function 
        |Success items -> 
            items
            |> Seq.choose id
            |> Seq.filter (fun si ->  Sln.isCsProj si.Path)
        | Failure s -> failwithf "%s" s
    //|> Result.bind (Result.choose id)
    |> List.ofSeq
    |> Seq.ofList
    //|> dumptif "slnProjects" debug

module Projects = 
    type XDocument with
        static member getRoot (x:XDocument) = x.Root
        static member loadRoot (path:string) = XDocument.Load(path).Root |> (fun r -> r.Name.Namespace,r)

    let getElements (elem:XElement) = elem.Elements()
    let getElementsByName (ns:XNamespace) name (elem:XElement) = elem.Elements(ns + name)
    let getItemGroups (root:XElement) = getElementsByName root.Name.Namespace "ItemGroup" root

    slnProjects 
    |> Seq.map (fun si -> si, si.Path.Value |> XDocument.Load |> XDocument.getRoot |> getItemGroups)
    |> dumptif "itemGroups" debug
    |> ignore

    let getCsFiles (FilePath projectPath as fp) = // did not account for item group conditions, item conditions, nor target/task generated/modified items
        let ns,root = XDocument.loadRoot projectPath
        getItemGroups root
        |> Seq.map getElements
        |> Seq.collect id
        |> Seq.choose (fun n -> 
            if n.Name.LocalName = "Compile" && n.Attribute(XNamespace.None + "Include").Value |> endsWith ".cs" then 
                n.Attribute(XNamespace.None+"Include").Value |> Some 
            else None)
        //|> dumpt "Items"
        
type MethodInfo = {SpanStart:int; SpanEnd:int; Text:string}
type FileInfo = {Path:FilePath; Methods: MethodInfo seq}
type ProjectFileInfo = {Name:string; Fp:FilePath; Files: FileInfo seq}

slnProjects |> Seq.length |> dumptif "checking x projects" debug |> ignore


let projectFileMethods = 
    slnProjects
    |> Seq.map (fun si -> 
        let projFolder = combine (Path.GetDirectoryName si.Path.Value)
        {Name=si.Name; Fp=si.Path;Files = 
            si.Path 
            |> Projects.getCsFiles 
            |> dumptif "project cs files!" debug
            |> Seq.map (tee (projFolder >> FilePath))
            |> Seq.choose (fun (fp, relPath) -> match fp with | Success fp -> Some (relPath,fp) | Failure s -> dumph s; None)
            |> Seq.map (fun (relPath, fp) -> fp, 
                                                fp 
                                                |> BRoslyn.getTree 
                                                |> BRoslyn.getRoot 
                                                // get all methods declared in file
                                                |> BRoslyn.Methods.getMethods 
                                                // find methods that are not overrides (I want to address the root, not the fallout)
                                                |> Seq.filter(BRoslyn.Methods.isOverride >> not) 
                                                // find methods that have a try inside them
                                                |> Seq.filter BRoslyn.Methods.hasTry // TODO: check for catch without re-throw, not just any try
                                                // find method that don't mention try in the name
                                                |> Seq.filter (BRoslyn.Methods.isTryMethodProperlyNamed >> not )
                                                |> List.ofSeq
                                            )
            |> Seq.filter (snd >> any) // fun (_, methods) -> Seq.exists (fun _ -> true) methods)
            |> Seq.map (fun (fp, methods) -> fp, methods 
                                                    |> Seq.map(fun m -> m.SpanStart, m.Span.End, m.GetText().ToString()
//                                                                            , 
//                                                                            m.DescendantNodes()
//                                                                            |> Seq.filter ( fun n -> n.IsKind(SyntaxKind.ConditionalExpression)) 
//                                                                            //|> Seq.map(fun n -> n.ToString().Dump(); n)
//                                                                            |> List.ofSeq
                                                                ) 
                                                                            
                                                                            )
                        
            |> Seq.map (fun (fp, methods) -> {Path= fp; Methods = methods|> Seq.map (fun (spanStart, spanEnd, text) -> {SpanStart=spanStart; SpanEnd= spanEnd; Text=text})})
            |> List.ofSeq
        }
    )
    |> List.ofSeq
    |> dumptif "got cs files" debug

module Dte = 
    let getDte() = System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE.14.0") :?> EnvDTE.DTE
    let openFile (dte:EnvDTE.DTE) path = dte.ExecuteCommand("File.OpenFile", path) //@"C:\TFS\PracticeManagement\dev\PracticeManagement\PracticeManagement\Billing\PatientDataGridControlViewModel.cs"

projectFileMethods
|> Seq.map (fun m -> m.Name, m.Files |> Seq.map (fun fi -> fi.Path.Value,LINQPad.Hyperlinq(fi.Path.Value,"Open"),LINQPad.Hyperlinq(new System.Action(fun () -> Dte.openFile (Dte.getDte()) fi.Path.Value), "Open In Vs2015"), fi.Methods),
                                        m.Fp.Value
            )
|> dumpt "projects to files with badly named try methods"
//|> Seq.map (fun (projName,projPath, methods |> Seq.map (fun (relPath,methods))))
