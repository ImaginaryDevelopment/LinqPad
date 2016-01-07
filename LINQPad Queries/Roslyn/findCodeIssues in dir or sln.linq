<Query Kind="FSharpProgram">
  <NuGetReference>Microsoft.CodeAnalysis</NuGetReference>
  <NuGetReference>Microsoft.CodeAnalysis.CSharp</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.CodeAnalysis</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp.Syntax</Namespace>
</Query>

// find code issues like methods that have a try but don't say so in the method name

//with help from https://github.com/dotnet/roslyn/wiki/Getting-Started-C%23-Semantic-Analysis
//translate to F# so that we can choose all files under directory, or all files in all projects in a solution

// if we decide to have the file/method auto open on click in the output this is the hook
// var devEnvs = System.Diagnostics.Process.GetProcessesByName("devenv"); //.Dump();

let debug = false

[<AutoOpen>]
module LinqPad =
    let dumpt (title:String) x = x.Dump(title); x
    let dumptif (title:String) b x = if b then dumpt title x else x
    
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
    |> dumptif "slnProjects" debug

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
        |> Seq.choose (fun n -> if n.Name.LocalName = "Compile" && n.Attribute(XNamespace.None + "Include").Value |> endsWith ".cs" then n.Attribute(XNamespace.None+"Include").Value |> Some else None)
        //|> dumpt "Items"
    
    slnProjects
    |> Seq.map (fun si -> 
        let projFolder = combine (Path.GetDirectoryName si.Path.Value)
        si.Name, 
            si.Path |> getCsFiles |> Seq.map (tee (projFolder >> FilePath))
            |> Seq.choose (fun (fp,relPath) -> match fp with | Success fp -> Some (fp,relPath) | Failure s -> None)
            |> Seq.map (fun (fp,relPath) -> fp,relPath)
    )
    |> dumpt "got cs files"
    
//Directory.GetFiles(target, "*.cs", SearchOption.AllDirectories)
//    .Where(fun fp -> not <| fp.EndsWith(".g.cs") && not <| fp.EndsWith(".g.i.cs") && not <| fp.Contains("\\obj\\"))
//|> List.ofSeq
//|> dumpt "files"
//|> Seq.map FilePath
//|> Seq.choose id

//|> Seq.map (tuplef (getTree >> getRoot))
//|> Seq.map (tuplef1 getRoot)

//|> Seq.collect id 
//(WalkFile).Dump()
//|> dumpt "walked"

// begin C# comments
//IEnumerable<object> WalkFile(string filePath)
//{
//    SyntaxTree tree = CSharpSyntaxTree.ParseText(File.ReadAllText(filePath));
//
//    var root = (CompilationUnitSyntax)tree.GetRoot();
//
//
//    var methods = root.DescendantNodes()
//                               .OfType<MethodDeclarationSyntax>()
//                               .ToArray();
//    if (methods == null || !methods.Any())
//        yield break;
//    var tryMethods =
//        methods
//            .Where(m =>m.Modifiers.Any(mo => mo.Text != "override") && m.DescendantNodes().OfType<TryStatementSyntax>().Any())
//            .Select(m => new {Identifier = m.Identifier.Text,Modifiers=m.Modifiers, Body= m.GetText().ToString()  })
//            .ToArray();
//    if (!tryMethods.Any())
//        yield break;
//    yield return new { File = filePath, TryMethods = tryMethods};
//
////}
//
//void DoCompilation(SyntaxTree tree)
//{
//    var root = (CompilationUnitSyntax)tree.GetRoot();
//    var compilation = CSharpCompilation.Create("HelloWorld")
//                                         .AddReferences(
//                                              MetadataReference.CreateFromFile(
//                                                  typeof(object).Assembly.Location))
//                                         .AddSyntaxTrees(tree);
//    var model = compilation.GetSemanticModel(tree);
//    var nameInfo = model.GetSymbolInfo(root.Usings[0].Name);
//    var systemSymbol = (INamespaceSymbol)nameInfo.Symbol;
//    var members = systemSymbol.GetNamespaceMembers().Select(s => s.Name);
//}

// ------------------------------------------------------------------
// end c# comments
// ------------------------------
type FileTarget = 
    |AllFiles of dirPath:String
    |SolutionProjectFiles of slnPath:String
