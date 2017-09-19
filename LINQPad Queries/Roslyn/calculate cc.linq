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

type VisitMessage = 
    | Class of name:string
    | Method of className:string * name:string
    | Branch of className:string*methodName:string * bType:string * line:int * extra:string
    

type ModelCollector private (fMsg) =
    inherit CSharpSyntaxWalker()
    let mutable className = null
    //let mutable simpleClassName = null
    let mutable methodName = null
    let addBranch titling (node:#SyntaxNode) (f:_ -> obj) =        
        // lines seem to be off by 1
        let line = node.GetLocation().GetLineSpan().StartLinePosition.Line + 1                
        Branch(className, methodName, titling, line, f node |> string)
        |> fMsg
        
        
    let visitNode (node:SyntaxNode) =         
        match node.Kind() with
        | SyntaxKind.CaseSwitchLabel 
        | SyntaxKind.CoalesceExpression
        | SyntaxKind.ConditionalExpression
        | SyntaxKind.LogicalAndExpression
        | SyntaxKind.LogicalOrExpression
        | SyntaxKind.LogicalNotExpression as x ->
                //addBranch "VisitKind" node (fun _ -> box x)
                ()
        | _ -> ()

    // not sure the value this provides, but it was used in the sample
    override x.Visit node =
        
        base.Visit node
        visitNode node
    // this may be the wrong thing to match with Vs code metrics
    override __.VisitParenthesizedLambdaExpression node = 
        addBranch "Lambda" node (fun node -> box node.ParameterList)
        base.VisitParenthesizedLambdaExpression node
        
    override __.VisitAnonymousMethodExpression node = 
        addBranch "AnonymousMethod" node box
        base.VisitAnonymousMethodExpression node
        
    override __.VisitClassDeclaration node = 
        // handle nested classes
        let oldCn = className
        //let oldSCn = simpleClassName
        //simpleClassName <- node.Identifier.ValueText
        let cn = 
            //simpleClassName
            node.Identifier.ValueText
            |> (+) (if isNull oldCn then String.Empty else oldCn + ".")
        VisitMessage.Class cn
        |> fMsg

        className <- cn
        base.VisitClassDeclaration node
        className <- oldCn
        //simpleClassName <- oldSCn
        
    override __.VisitWhileStatement node =
        addBranch "While" node (fun n -> box n.Condition)
        base.VisitWhileStatement node
        
    override __.VisitForStatement node = 
        addBranch "For" node (fun n -> box n.Condition)
        base.VisitForStatement node
            
    override __.VisitForEachStatement node = 
        addBranch "ForEach" node (fun f -> sprintf "%s in %O" f.Identifier.ValueText f.Expression |> box)
        base.VisitForEachStatement node      
        
//    override __.VisitCaseSwitchLabel node = 
//        addBranch "SwitchLabel" node box
//        base.VisitCaseSwitchLabel node
        
    override __.VisitConditionalExpression node = 
        addBranch "CondExpr" node (fun f -> box f.Condition)
        base.VisitConditionalExpression node
        
    override __.VisitSwitchSection node = 
        node.Labels
        |> Seq.iter(fun node -> 
            addBranch "SwitchSection" node box
        )
        base.VisitSwitchSection node
        
    override __.VisitElseDirectiveTrivia node =
        addBranch "ElseD" node (fun _ -> box String.Empty)
        base.VisitElseDirectiveTrivia node

    // isn't an else, somewhat always implied? guess it depends on if the if true branch contains an early return
    override __.VisitElseClause node =
        addBranch "Else" node (fun _ -> box String.Empty)
        base.VisitElseClause node

    // what even is this?    
    override __.VisitIfDirectiveTrivia node = 
        addBranch "IfD" node (fun _ -> box node.Condition)
        base.VisitIfDirectiveTrivia node
        
    override __.VisitContinueStatement node = 
        addBranch "Continue" node box
    override __.VisitGotoStatement node = 
        addBranch "Goto" node box
    override __.VisitIfStatement node = 
        addBranch "If" node (fun f -> box f.Condition)
        base.VisitIfStatement node
        
    override __.VisitCatchClause node = 
        addBranch "Catch" node (fun _ -> box node.Declaration)
        base.VisitCatchClause node

    (* since we get no intellisense for override declarations *)
    override __.VisitMethodDeclaration node =        
    
        // in case method declarations nest (anonymous methods or lambdas)
        
        let oldMethodName = methodName
        methodName <- node.Identifier.ValueText
        //printfn "Visiting method %s.%s" className methodName
        Method(className,methodName)
        |> fMsg
        base.VisitMethodDeclaration node
        methodName <- oldMethodName
        
    override x.VisitPropertyDeclaration node = 
        let oldMethodName = methodName
        methodName <- node.Identifier.ValueText
        base.VisitPropertyDeclaration node
        methodName <- oldMethodName
        
    override __.VisitAccessorDeclaration node = 
        let oldMethodName = methodName
        match node.Kind() with
        | SyntaxKind.SetAccessorDeclaration ->
            sprintf "%s.set" oldMethodName
        | SyntaxKind.GetAccessorDeclaration ->
            sprintf "%s.get" oldMethodName
        | x -> sprintf "%s.%s" oldMethodName (x |> string)
        |> fun x -> methodName <- x
        base.VisitAccessorDeclaration node
        methodName <- oldMethodName
        
    override __.VisitConstructorDeclaration node = 
        let oldMethodName = methodName
        methodName <- "new"
        base.VisitConstructorDeclaration node
        methodName <- oldMethodName
        
    static member VisitMcm (text:string) = 
        // classname -> methodName -> cc
        let items = Dictionary<string,Dictionary<string,ResizeArray<string>>>()
        let tree = CSharpSyntaxTree.ParseText text
        let root = tree.GetRoot() :?> CompilationUnitSyntax
        let addClass cn = 
            let cn = if String.IsNullOrWhiteSpace cn then String.Empty else cn
            if not <| items.ContainsKey cn then
                items.[cn] <- Dictionary<string,ResizeArray<_>>()
                cn
            else cn
        let addMethod cn mn = 
            let cn = addClass cn
            let mn = if String.IsNullOrWhiteSpace mn then String.Empty else mn
            if not <| items.[cn].ContainsKey mn then
                items.[cn].[mn] <- ResizeArray<_>()
            mn
        let fMsg = 
            (function
                            | Class cn -> addClass cn |> ignore
                            | Method (cn,mn) ->
                                let cn = addClass cn
                                addMethod mn |> ignore
                            | Branch(cn,mn,bType,line,extra) ->
                                let cn = addClass cn
                                let mn = addMethod cn mn
                                sprintf "%i: %s (%s)" line bType extra
                                |> items.[cn].[mn].Add
                                |> ignore)
        let mc = ModelCollector(fMsg)
        mc.Visit(root)
        items
let pathFilter (path:string) = 
    not <| path.Contains "CodedUITest"
    && not <| path.Contains "ViewModelsC"
    && not <| path.Contains "Pm.TestsC"
    && not <| path.Contains "Packages"
    && not <| path.Contains "node_modules"
    && not <| path.EndsWith "AssemblyInfo.cs"

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
        let methods = ModelCollector.VisitMcm code
        f, ModelCollector.VisitMcm code, methods
    )
    |> Seq.map(fun (f, branches, methods) ->
        let cc = 
            branches 
            |> Seq.map (|KeyValue|) 
            |> Seq.map (fun (cn,v) -> 
                let classCC = v |> Seq.length |> (+) 1
                let ccByMethod = 
                    v 
                    |> Seq.map (|KeyValue|) 
                    |> Seq.map (fun (mn, items) ->
                        mn, items |> Seq.length |> (+) 1, items
                    )
                    |> Seq.sortByDescending (fun (_,l,_) -> l)
                    |> List.ofSeq
                cn, classCC, ccByMethod
                
            )
        let fileCC = cc |> Seq.map(fun (_,classCC,_) -> classCC) |> Seq.sum
        f, fileCC, cc //,branches
    )
    |> Seq.sortByDescending (fun (_,cc,_) -> 
        cc
    )
    
    //.Where(x => x.Properties.Any())
    |> Seq.truncate 10
    |> Dump
    |> ignore