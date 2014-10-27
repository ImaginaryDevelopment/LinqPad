#r "System.Runtime"
#r "System.IO"
#r "System.Collections"
#r "System.Text.Encoding"
#r "System.Threading.Tasks"
#r @"C:\Program Files (x86)\Reference Assemblies\Microsoft\MSBuild\v12.0\Microsoft.Build.Utilities.v12.0.dll"
// #r "Microsoft.Build"
System.Reflection.Assembly.GetExecutingAssembly().GetReferencedAssemblies()
|> Seq.iter ( fun a ->printfn "assembly loaded name %A" a)

#if _
#r "System.Collections.Immutable.dll"
#r "Microsoft.CodeAnalysis.dll"
#r "Microsoft.CodeAnalysis.CSharp.dll"
#r "Microsoft.CodeAnalysis.Workspaces.dll"
#r "Microsoft.CodeAnalysis.Desktop.dll"
#r "System.Reflection.Metadata.dll"
#else
#endif

open System
open System.IO
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.MSBuild

// find all ifs that have the return on the same line
// http://www.dreamincode.net/forums/blog/217/entry-4757-improving-the-code-smell/

// http://blog.filipekberg.se/2011/10/20/using-roslyn-to-parse-c-code-files/

// http://source.roslyn.codeplex.com/

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

//adjust your preference here
let devRoot = Environment.ExpandEnvironmentVariables("%devroot%")

let slnPath = 
    System.IO.Directory.GetFiles(devRoot,"*.sln",SearchOption.AllDirectories)
    // hack to avoid hard coding a specific sln in a general solution
    |> Seq.skipWhile(fun d->d.Contains("Playground")=false)
    |> Seq.skipWhile(fun d->d.Contains("Playground"))
    |> Seq.head
    |> fun e-> 
    #if LINQPAD
        e.Dump("sln file")
    #endif
        e

    
// active pattern : http://stackoverflow.com/a/25243799/57883
let (|As|_|) (p:'T) : 'U option =
    let p = p :> obj
    if p :? 'U then Some (p :?> 'U) else None    
let workspace = 
    try
        MSBuildWorkspace.Create()    
    with | :? System.Reflection.ReflectionTypeLoadException as rtle -> printfn "LoaderExceptions: %A" rtle.LoaderExceptions; reraise()
let solution:Solution =
    try

        let asyncSln = workspace.OpenSolutionAsync(slnPath)
        asyncSln.RunSynchronously()
        asyncSln.Result
    with | :? System.TypeLoadException as tle -> printfn "TypeLoadException on solution %s %A" slnPath tle.TypeName; reraise()

//    
//    try
//        Solution.Load(slnPath)
//    with ex -> 
//        if ex.Message = "Expected Global line." then 
//            "check for a line like this in your .sln \"VisualStudioVersion = \" or \"MinimumVisualStudioVersion = \"".Dump()
//            LINQPad.Hyperlinq("https://social.msdn.microsoft.com/Forums/en-US/df9af25e-75fc-47e6-9704-8aa89817857c/systemexception-expected-global-line", "help topic").Dump()
//            
//        reraise()

let checkforCountIssue document (node:SyntaxNode) =
    match node with
    | As (binaryExpression:BinaryExpressionSyntax) ->
        let isCallToEnumerableCount (document:Document, expr:ExpressionSyntax) = 
            let invocation = match expr with
                                | As (i:InvocationExpressionSyntax) -> Some i
                                | _ -> None
            if invocation.IsNone then
                false
            else
                let semanticModel = document.GetSemanticModelAsync().Result
                let methodSymbol = semanticModel.GetSymbolInfo(expr)
                let ms = methodSymbol.Symbol.OriginalDefinition
                if ms = null || ms.Name <> "Count" (* TODO: || ms.ConstructedFrom = null *) then
                    false
                else
                    let enumerable = semanticModel.Compilation.GetTypeByMetadataName(typeof<System.Linq.Enumerable>.FullName)
                    enumerable <> null (* TODO && ms.ConstructedFrom.ContainingType.Equals(enumerable) *)
                        
        let isRelevantComparison (document:Document, expression:ExpressionSyntax, validator:int->bool) =
            let semantic = document.GetSemanticModelAsync().Result
            let symbolInfo = semantic.GetSymbolInfo(expression)
            let constant = semantic.GetConstantValue(expression)
            if not constant.HasValue then false
            else
                let value = constant.Value :?> int
                validator(value)
        
        let isRelevantRightSideComparison(document:Document, expression:ExpressionSyntax, kind) =
            isRelevantComparison(
                    document, expression, 
                    ( fun v -> kind = SyntaxKind.GreaterThanExpression && v = 0 || kind = SyntaxKind.GreaterThanOrEqualExpression && v = 1)
                    
                )
                        
        let isRelevantLeftSideComparison(document:Document, expression:ExpressionSyntax, kind) =
            isRelevantComparison(
                document,
                expression,
                (fun v -> kind = SyntaxKind.LessThanExpression && v = 0 || kind = SyntaxKind.LessThanOrEqualExpression && v = 1)
                
            )
        let left,right,kind = binaryExpression.Left, binaryExpression.Right, binaryExpression.OperatorToken.CSharpKind()

        seq{
            if isCallToEnumerableCount(document, left) && isRelevantRightSideComparison(document, right, kind) ||
                isCallToEnumerableCount(document,right) && isRelevantLeftSideComparison(document,left,kind) then
                    yield Diagnostic.Create("OSX1000","Performance",sprintf "Change %A to use .Any() instead of .Count() to avoid possible enumeration of entire sequence." binaryExpression,DiagnosticSeverity.Info, true, 1, false)
        }
    | _ -> Seq.empty
    
// printfn "end of line trivia is %i" (int(SyntaxKind.EndOfLineTrivia)) 8395
// failed to get this to work https://roslyn.codeplex.com/discussions/570098#editor for non-regex, should try from C# with some intellisense
type System.String with
    member x.IsMatch (pattern,ignoreCase) = 
        let caseOption = if ignoreCase then System.Text.RegularExpressions.RegexOptions.IgnoreCase else System.Text.RegularExpressions.RegexOptions.None
        System.Text.RegularExpressions.Regex.IsMatch(x,pattern, caseOption)

let getSingleLineIfReturns (node:SyntaxNode) =
    let ifReturnPattern = @"if\s*\(.*\).*return "
    seq {
        match node with
        | As (ifStatementNode:IfStatementSyntax) ->
            if ifStatementNode.ChildNodes() |> Seq.ofType<ReturnStatementSyntax> |> Seq.any then
                let text = ifStatementNode.GetText().ToString()
                let isMatch = 
                    let caseOption = System.Text.RegularExpressions.RegexOptions.None
                    System.Text.RegularExpressions.Regex.IsMatch(text,ifReturnPattern, caseOption)
                if isMatch then
                    let message = sprintf "Single line ifs are not allowed %A" ifStatementNode
                    let create (id:string) (category:string) (msg:string) = Diagnostic.Create(id,category,msg,DiagnosticSeverity.Warning, true, 1, false)
                    let diagnostic = create "CVS1000" "Style" message
                    yield diagnostic
        | _ -> ()
    }
let getIssues(document:Document, node:SyntaxNode) : Diagnostic seq =
    seq{
        yield! checkforCountIssue document node
        yield! getSingleLineIfReturns node
    }
    
type IssueInformation = {ProjectName:string; DocumentName:string;IssueNodes:Diagnostic seq}                

let pData = 
    seq {
        let filteredProjects = 
            solution.Projects 
        for p in filteredProjects do
            for d in p.Documents do
                let semantic = d.GetSemanticModelAsync().Result
                let tree = semantic.SyntaxTree
                let nodes = 
                    tree.GetRoot().DescendantNodes()
                for node in nodes do
                    let issues = getIssues(d,node)
                    if issues |> Seq.any then
                        yield {IssueInformation.ProjectName = p.Name; DocumentName= d.Name; IssueNodes= issues}
    }

    
pData 
//|> Seq.head 
#if LINQPAD
|> Dump
#else
|> (fun data -> printfn "%A" data)
#endif