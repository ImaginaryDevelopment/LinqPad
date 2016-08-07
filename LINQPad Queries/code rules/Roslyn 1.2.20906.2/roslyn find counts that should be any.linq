<Query Kind="FSharpProgram">
  <NuGetReference>Microsoft.CodeAnalysis</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>Microsoft.CodeAnalysis</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp.Syntax</Namespace>
  <Namespace>System.Reactive.Subjects</Namespace>
</Query>

// currently just finds call to Count() that should be Any()
// intention: find all ifs that have the return on the same line
// currently broken with Roslyn changes/updates
// http://blog.filipekberg.se/2011/10/20/using-roslyn-to-parse-c-code-files/
let dumpt (t:string) x = x.Dump(t); x
module Seq =
    let ofType<'a> (source : System.Collections.IEnumerable) : seq<'a> =
   		let resultType = typeof<'a>
   		seq {
            for item in source do
                match item with
                    | null -> ()
                    | _ ->
                    if resultType.IsAssignableFrom (item.GetType ())
                    then
                        yield (downcast item)
   		}
    let any source = 
        Seq.exists( fun f-> true) source

type StringTransform = (string->string)
type SearchOptions =
    |Absolute of string
    |RelativeFile of StringTransform
    |AbsoluteDir of string
    |UseEnvironmentalBase
    |RelativeDir of StringTransform
    |AskForAbsoluteDir of (unit-> string)

//adjust your preference here
let analysisSearchOption = SearchOptions.UseEnvironmentalBase
let devRoot = Environment.ExpandEnvironmentVariables("%devroot%")
let slnPath = 
    System.IO.Directory.GetFiles(devRoot,"*.sln",SearchOption.AllDirectories)
    |> Seq.skip 1
    |> Seq.head
    |> fun e-> e.Dump("sln file"); e

// active pattern : http://stackoverflow.com/a/25243799/57883
let (|As|_|) (p:'T) : 'U option =
    let p = p :> obj
    if p :? 'U then Some (p :?> 'U) else None	

// works but uncessary
// solution.Dump()

let isCallToEnumerableCount (fGetSymbolInfo,fGetTypeByMetadataName, expr:ExpressionSyntax, cancellationToken) = 
    let invocation = match expr with
                        | As (i:InvocationExpressionSyntax) -> Some i
                        | _ -> None
    if invocation.IsNone then
        false
    else
        let methodSymbol:SymbolInfo = fGetSymbolInfo(expr,cancellationToken) //semanticModel.GetSymbolInfo(expr, cancellationToken)
        let ms = methodSymbol.Symbol :?> IMethodSymbol
        if ms = null || ms.Name <> "Count" || ms.ConstructedFrom = null then
            false
        else
            // used to return INamedTypeSymbol
            let enumerable : ISymbol = fGetTypeByMetadataName() // semanticModel.Compilation.GetTypeByMetadataName(typeof<Enumerable>.FullName)
            enumerable <> null && ms.ConstructedFrom.ContainingType.Equals(enumerable)
                
let isRelevantComparison (fGetSymbolInfo,fGetConstantValue, validator:int->bool,token) =
    let symbolInfo:SymbolInfo = fGetSymbolInfo () //semanticModel.GetSymbolInfo(expression)
    let constant:Optional<obj> = fGetConstantValue () //semanticModel.GetConstantValue(expression)
    if not constant.HasValue then false
    else
        let value = constant.Value :?> int
        validator(value)

let isRelevantRightSideComparison(fGetSymbolInfo,fGetConstantValue, kind,token) =
    isRelevantComparison(
            fGetSymbolInfo,fGetConstantValue,
            ( fun v -> kind = SyntaxKind.GreaterThanExpression && v = 0 || kind = SyntaxKind.GreaterThanOrEqualExpression && v = 1),
            token
        )
                
let isRelevantLeftSideComparison(fGetSymbolInfo,fGetConstantValue, kind,token) =
    isRelevantComparison(
        fGetSymbolInfo,fGetConstantValue,
        (fun v -> kind = SyntaxKind.LessThanExpression && v = 0 || kind = SyntaxKind.LessThanOrEqualExpression && v = 1),
        token
    )
    
//typeof<DiagnosticDescriptor>.GetConstructors().Dump()
let checkforCountIssue fObserver (fGetSymbolInfo,fGetConstantValue,fGetTypeByMetadataName) (node:ExpressionSyntax) token =
    // borrowing a working version from https://msdn.microsoft.com/en-us/magazine/dn904670.aspx and using dummy(maybe inappopriate) arguments
    let rule = new DiagnosticDescriptor(id = "IEnumerable",title = "Used .Count() instead of .Any()", messageFormat = "IEnumerableError {0}", category="Syntax", defaultSeverity = DiagnosticSeverity.Error, isEnabledByDefault = true, description="", helpLinkUri="", customTags=[| |])
    //rule.IsEnabledByDefault <- true
    let binaryExpression = node :?> BinaryExpressionSyntax
    binaryExpression.Kind() |> string |> fObserver
    let left,right,kind = binaryExpression.Left, binaryExpression.Right, binaryExpression.Kind()
    let getLeftConstantValue () = fGetConstantValue left
    let getRightConstantValue () = fGetConstantValue right
    seq{
        let fGetRightSymbolInfo () = fGetSymbolInfo(right,token)
        let fGetLeftSymbolInfo () = fGetSymbolInfo(left,token)
        if isCallToEnumerableCount(fGetSymbolInfo,fGetTypeByMetadataName, left, token) && isRelevantRightSideComparison(fGetRightSymbolInfo,getRightConstantValue, kind, token) ||
            isCallToEnumerableCount(fGetSymbolInfo,fGetTypeByMetadataName,right,token) && isRelevantLeftSideComparison(fGetLeftSymbolInfo,getLeftConstantValue,kind,token) then
                yield Diagnostic.Create(rule, binaryExpression.GetLocation())
                //was : CodeIssue(CodeIssueKind.Info, binaryExpression.Span,sprintf "Change %A to use Any() instead of Count() to avoid possible enumeration of entire sequence." binaryExpression)
    }
    
let getIssues(fWatcher:string -> unit,fGetSymbolInfo,fGetConstantValue,fGetTypeByMetadataName, node:ExpressionSyntax, token:CancellationToken) =
    seq{
        yield! checkforCountIssue fWatcher (fGetSymbolInfo,fGetConstantValue,fGetTypeByMetadataName) node token
    }
    
type IssueInformation = {ProjectName:string; DocumentName:string;IssueNodes:Diagnostic seq}		

let createBehavior title v =
    let result = new BehaviorSubject<'t>(v)
    result.DumpLatest(description=title) |> ignore
    result

let binaryExpressionKindSub = createBehavior "binaryExpressionKind" ""
let documentAnalysisTime = createBehavior "LastDocumentAnalysisTime" TimeSpan.Zero
let semanticModelTime = createBehavior "LastSemanticModelTime" ("",TimeSpan.Zero)
let currentProjectSub = createBehavior "currentProject" ""
let currentDocumentSub = createBehavior "currentDocument" ""
let binaryNodeCount =  createBehavior "binaryNodeCount" 0
let currentNodeSub = createBehavior "CurrentBinaryNode" ""

currentProjectSub.Dump() // show started and by proxy, finished project analyses
currentDocumentSub.Dump()
// binaryNodeCount.Dump("Dumped") // dumps list of elements as they occur
let pData = 
    // about workspaces: https://joshvarty.wordpress.com/2014/09/12/learn-roslyn-now-part-6-working-with-workspaces/
    let msWorkspace = Microsoft.CodeAnalysis.MSBuild.MSBuildWorkspace.Create()
    //msWorkspace.LoadMetadataForReferencedProjects <- true
    msWorkspace.SkipUnrecognizedProjects <- true
    //msWorkspace.WorkspaceChanged.Add ( fun t -> t.Dump("WorkspaceChanged"))
    msWorkspace.WorkspaceFailed.Add (fun t -> t.Dump("workspace failed"))
    let solution =
        try
            msWorkspace.OpenSolutionAsync(slnPath).Result
        with ex -> 
            if ex.Message = "Expected Global line." then 
                "check for a line like this in your .sln \"VisualStudioVersion = \" or \"MinimumVisualStudioVersion = \"".Dump()
            LINQPad.Hyperlinq("https://social.msdn.microsoft.com/Forums/en-US/df9af25e-75fc-47e6-9704-8aa89817857c/systemexception-expected-global-line", "help topic").Dump()
            reraise()
    let sw = Stopwatch()
//    let tryGetProject filePath = 
//        try
//            msWorkspace.OpenProjectAsync(filePath).Result |> Some
//        with 
//            | :? AggregateException as ex when ex.InnerExceptions.[0].Message.EndsWith("is already part of the workspace.") -> None
//            | x -> reraise()
            
    //seq {
    let filteredProjects = solution.Projects 
    //let filteredCompiledProjects = 
    //    filteredProjects |> Seq.map (fun p-> p.GetCompilationAsync()) |> Array.ofSeq
    
    //let filteredCompiledProjects = System.Threading.Tasks.Task.WhenAll(filteredCompiledProjects)
//    "finished compilation?".Dump()
    for p in filteredProjects do
        
        //let project = tryGetProject p.FilePath |> Option.iter(dumpt "Project opened!" >> ignore)
        //let item = Async.RunSynchronously (msWorkspace.opendo
        currentProjectSub.OnNext(p.Name)
        "Starting compilation".Dump()
        let compilation =  p.GetCompilationAsync().Result
        compilation.Dump("Compilation")
        for d in p.Documents (* |> Seq.filter(fun d -> d.Name <> "ApplicationVariablesDataAccess.cs") *) do
            currentDocumentSub.OnNext d.Name
            // not sure if this is necessary
            
            if not d.SupportsSemanticModel then failwithf "did not support semantic models %s" d.Name
            if not d.Project.SupportsCompilation then failwithf "did not support compilation %s" d.Project.Name
            
            //let item = msWorkspace.OpenDocument(d.Id,true)
            //System.Diagnostics.Debugger.Break()
            sw.Start()
            let tree = d.GetSyntaxTreeAsync().Result
            tree.Dump("tree!")
            //d.Project.getcom
            
            
            let semantic = d.GetSemanticModelAsync()
            
            (semantic.Exception,semantic.AsyncState,semantic.Status, semantic.IsFaulted,semantic.IsCompleted, semantic.IsCanceled).Dump("semantic model before awaiting")
            //d.FilePath.Dump("started getSemanticModelAsync processing")
            
            let semantic = 
                try
                    semantic.Result
                with ex -> (semantic.Exception,semantic.AsyncState, semantic.Status, semantic.IsFaulted,semantic.IsCompleted, semantic.IsCanceled).Dump("semantic after exception"); reraise()
            semantic.Dump()
            let root = d.GetSyntaxRootAsync().Result
            let binarynodes = 
                root.DescendantNodes()
                |> Seq.ofType<BinaryExpressionSyntax>
                |> List.ofSeq
            sw.Stop()
            semanticModelTime.OnNext (d.Name,sw.Elapsed)
            
            //System.Diagnostics.Debugger.Break()
            sw.Start()  
            
//                for binaryNode in binarynodes do
//                    binaryNodeCount.OnNext(binaryNodeCount.Value + 1)
//                    currentNodeSub.OnNext(binaryNode.ToString())
//
//                    //let getIssues(fWatcher:string -> unit,fGetSymbolInfo,fGetConstantValue,fGetTypeByMetadataName, node:ExpressionSyntax, token:CancellationToken) =
//                    //let issues = getIssues(binaryExpressionKindSub.OnNext, semantic, binaryNode, CancellationToken()) |> List.ofSeq
//                    let getIssues watcher fGetSymbolInfo fGetConstantValue fGetTypeByMetadata node token= 
//                        getIssues(watcher, fGetSymbolInfo, fGetConstantValue, fGetTypeByMetadata, node, token)
//                    let fGetTypeByMetadataName () = semantic.Compilation.GetTypeByMetadataName(typeof<Enumerable>.FullName) :> ISymbol
//                    let getIssues token node = getIssues binaryExpressionKindSub.OnNext (semantic.GetSymbolInfo) (fun expr -> semantic.GetConstantValue(expr,token)) fGetTypeByMetadataName node token 
//                    let issues = 
//                        let token = CancellationToken()
//                        getIssues token binaryNode |> List.ofSeq
//                    if issues |> Seq.any then
//                        yield {IssueInformation.ProjectName = p.Name; DocumentName= d.Name; IssueNodes= issues}
            sw.Stop()
            documentAnalysisTime.OnNext(sw.Elapsed)
            sw.Reset()
//    }
//    |> List.ofSeq

    
pData 
//|> Seq.head 
|> Dump