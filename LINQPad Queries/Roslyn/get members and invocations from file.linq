<Query Kind="FSharpProgram">
  <NuGetReference>Microsoft.CodeAnalysis.CSharp</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>System.Collections.Immutable</NuGetReference>
  <Namespace>Microsoft.CodeAnalysis</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp.Syntax</Namespace>
</Query>

// cc = Edges - Nodes + 2 * ExitPoints
// for a single method ExitPoints is always 1 for some reason
// not going to account for empty if/else blocks for now at least
// reference https://stackoverflow.com/questions/9097987/calculation-of-cyclomatic-complexity
// it appears we need to but probably won't at the start, account for blocks before the if block and after
// reference of calculations using roslyn
// https://github.com/jjrdk/ArchiMetrics/blob/master/src/ArchiMetrics.Analysis/Metrics/CyclomaticComplexityCounter.cs
(* includes:
SyntaxKind.CaseSwitchLabel, SyntaxKind.CoalesceExpression, SyntaxKind.ConditionalExpression, SyntaxKind.LogicalAndExpression, SyntaxKind.LogicalOrExpression, SyntaxKind.LogicalNotExpression
*)

type ModelCollectionMode = 
    | Properties
    | Methods
    | MethodInvocations
    | Branching of includeKinds:bool
    member x.isBranching = 
        match x with
        | Branching _ -> true
        | _ -> false
    
// mutate the items
module Walking = 
    let rec findClassParent (node:SyntaxNode) = 
        match node.Parent with
        | :? ClassDeclarationSyntax as cds -> cds
        | null -> null
        | _ -> findClassParent(node.Parent)
open Walking

type ModelCollector private (mcm, items:Dictionary<string, ResizeArray<string>>) =
    inherit CSharpSyntaxWalker()
    let addToLookup key value = 
        if not <| items.ContainsKey key then
            items.[key] <- ResizeArray()
        items.[key].Add value
    let addBranch titling node f =
        match mcm with
        | Branching _ -> 
            let classNode = findClassParent node
            // lines seem to be off by 1
            let line = node.GetLocation().GetLineSpan().StartLinePosition.Line + 1
            f node
            |> sprintf "%i: %s(%O)" line titling
            |> addToLookup classNode.Identifier.ValueText
        | _ -> ()
        
    let visitNode (node:SyntaxNode) = 
        match mcm with
            | Branching true -> 
                match node.Kind() with
                | SyntaxKind.CaseSwitchLabel 
                | SyntaxKind.CoalesceExpression
                | SyntaxKind.ConditionalExpression
                | SyntaxKind.LogicalAndExpression
                | SyntaxKind.LogicalOrExpression
                | SyntaxKind.LogicalNotExpression as x ->
                    
                        addBranch "VisitKind" node (fun f -> x)
                | _ -> ()
            | _ -> ()
            
    // not sure the value this provides, but it was used in the sample
    override x.Visit node =
        base.Visit node
        visitNode node
            
    override __.VisitForEachStatement node = 
        addBranch "ForEach" node (fun f -> sprintf "%s in %O" f.Identifier.ValueText  f.Expression)
        base.VisitForEachStatement node
        
    override __.VisitForStatement node = 
        addBranch "For" node (fun f -> f.Condition)
        base.VisitForStatement node
        
    override __.VisitWhileStatement node =
        addBranch "While" node (fun f -> f.Condition)
        base.VisitWhileStatement node
        
    override __.VisitCaseSwitchLabel node = 
        addBranch "SwitchLabel" node id
        base.VisitCaseSwitchLabel node
        
    override __.VisitConditionalExpression node = 
        addBranch "CondExpr" node (fun f-> f.Condition)
        base.VisitConditionalExpression node
        
    override __.VisitSwitchSection node = 
        node.Labels
        |> Seq.iter(fun node -> 
            addBranch "SwitchSection" node (fun l ->
                l
            )
        )
        base.VisitSwitchSection node
    override __.VisitElseDirectiveTrivia node =
        addBranch "ElseD" node (fun _ -> String.Empty)
        base.VisitElseDirectiveTrivia node
    // isn't an else, somewhat always implied? guess it depends on if the if true branch contains an early return
    override __.VisitElseClause node =
        addBranch "Else" node (fun _ -> String.Empty)
        base.VisitElseClause node
    // what even is this?    
    override __.VisitIfDirectiveTrivia node = 
        addBranch "IfD" node (fun _ -> node.Condition)
        base.VisitIfDirectiveTrivia node
    override __.VisitIfStatement node = 
        addBranch "If" node (fun f -> f.Condition)
        base.VisitIfStatement node
        ()
    override __.VisitCatchClause node = 
        addBranch "Catch" node (fun _ -> node.Declaration)
        base.VisitCatchClause node
            
    override __.VisitPropertyDeclaration node = 
        match mcm with
        | Properties ->
            let classNode = node.Parent :?> ClassDeclarationSyntax
            let className = classNode.Identifier.ValueText
            
            addToLookup className node.Identifier.ValueText
            (* to get character index in file:
                node.FullSpan.Start.Dump(); *)
        | _ -> ()
        base.VisitPropertyDeclaration node

    override __.VisitInvocationExpression node = 
        match mcm with
        | MethodInvocations ->
            match findClassParent node with 
            | null -> node.Dump("null parent")
            | classNode ->
                let className = classNode.Identifier.ValueText
                match node.Expression with
                | :? IdentifierNameSyntax as ins ->
                    sprintf "this.%s" ins.Identifier.ValueText
                    |> addToLookup className 
                | :? MemberAccessExpressionSyntax as maes ->
                    let parent = node.Parent
                    match maes.Expression with
                    | :? IdentifierNameSyntax as ins ->
                        let identifier = ins.Identifier.ValueText
                        sprintf "id:%s%s%s" identifier (string maes.OperatorToken) maes.Name.Identifier.ValueText
                        |> addToLookup className
                    | :? BaseExpressionSyntax ->
                        maes 
                        |> string
                        |> sprintf "base:%s"
                        |> addToLookup className
                    | :? MemberAccessExpressionSyntax ->
                        maes
                        |> string
                        |> addToLookup className
                    | :? ThisExpressionSyntax as tes ->
                        let tes' = tes.Token.ValueText
                        sprintf "%s%s%s" tes' (string maes.OperatorToken) maes.Name.Identifier.ValueText
                        |> addToLookup className
                    | :? ParenthesizedExpressionSyntax ->
                        maes
                        |> string
                        |> addToLookup className
                    | :? PredefinedTypeSyntax ->
                        if maes.Expression |> string <> "string" then
                            maes.Expression.Dump("unknown PredefinedTypeSyntax")
                    | :? InvocationExpressionSyntax ->
                        maes
                        |> string
                        |> sprintf "Ies:%s"
                        |> addToLookup className
                    | _ ->
                        maes.Expression.Dump("unknown maes")
                        maes
                        |> string
                        |> addToLookup className
                        ()
                | _ -> 
                    node.Expression.Dump("unknown invocation type")
                    ()
        | _ -> ()
                
        base.VisitInvocationExpression node


    (* since we get no intellisense for override declarations *)
    override __.VisitMethodDeclaration node = 
        match mcm with
        | Methods ->
            match node.Parent with
            | :? ClassDeclarationSyntax as classnode ->
                let className = classnode.Identifier.ValueText
                addToLookup className node.Identifier.ValueText
            | _ -> node.Dump("parent is not a class")
        | _ -> ()
        base.VisitMethodDeclaration node
        
    static member VisitMcm mode (text:string) = 
        let items = Dictionary<string,ResizeArray<string>>()
        let tree = CSharpSyntaxTree.ParseText text
        let root = tree.GetRoot() :?> CompilationUnitSyntax
        let mc = ModelCollector(mode, items)
        mc.Visit(root)
        items
let pathFilter (path:string) = 
    not <| path.Contains "CodedUITest"
    && not <| path.Contains "ViewModelsC"
    && not <| path.Contains "Pm.TestsC"
    && not <| path.Contains "Packages"
    && not <| path.Contains "node_modules"
    && not <| path.EndsWith "AssemblyInfo.cs"
let ``getProps,Methods,Invocations`` code = 
    let properties = ModelCollector.VisitMcm Properties code
    let methods = ModelCollector.VisitMcm Methods code
    let mi = ModelCollector.VisitMcm MethodInvocations code
    properties, methods, mi 
    //"C:\TFS\PracticeManagement\dev\PracticeManagement\PracticeManagementRW_local.sln"
let rootPath = @"C:\TFS\PracticeManagement\dev\PracticeManagement"
let propsSeq = 
    [
    // not supported
//        ".fs"
        ".cs"
    ]
    |> Seq.collect (fun ext -> Directory.GetFiles(rootPath, sprintf "*%s" ext, SearchOption.AllDirectories))
    |> Seq.filter (pathFilter)
    |> Seq.map (fun f -> 
        let code = File.ReadAllText f
        f, ModelCollector.VisitMcm (Branching false) code
    )
    |> Seq.map(fun (f, x) ->
        f, x |> Seq.map (|KeyValue|) |> Seq.map (fun (k,v) -> k, v |> Seq.length |> (+) 1),x
    )
    //.Where(x => x.Properties.Any())
    |> Seq.truncate 10
    |> Dump
    |> ignore
    
