<Query Kind="FSharpProgram">
  <NuGetReference>Microsoft.CodeAnalysis.CSharp</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>System.Collections.Immutable</NuGetReference>
  <Namespace>Microsoft.CodeAnalysis</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp.Syntax</Namespace>
</Query>

// mutate the items
type ModelCollectionMode = 
    | Properties
    | Methods
    | MethodInvocations
type ModelCollector private (mcm, items:Dictionary<string, ResizeArray<string>>) =
    inherit CSharpSyntaxWalker()
    let addToLookup key value = 
        if not <| items.ContainsKey key then
            items.[key] <- ResizeArray()
        items.[key].Add value
    
    override x.VisitPropertyDeclaration node = 
        match mcm with
        | Properties ->
            let classNode = node.Parent :?> ClassDeclarationSyntax
            let className = classNode.Identifier.ValueText
            
            addToLookup className node.Identifier.ValueText
            (* to get character index in file:
                node.FullSpan.Start.Dump(); *)
        | _ -> ()
        base.VisitPropertyDeclaration node
        
    static member private FindClassParent (node:SyntaxNode) = 
        match node.Parent with
        | :? ClassDeclarationSyntax as cds -> cds
        | null -> null
        | _ -> ModelCollector.FindClassParent(node.Parent)
       
    override __.VisitInvocationExpression node = 
        match mcm with
        | MethodInvocations ->
            match ModelCollector.FindClassParent node with 
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

    //"C:\TFS\PracticeManagement\dev\PracticeManagement\PracticeManagementRW_local.sln"
let rootPath = @"C:\TFS\PracticeManagement\dev\PracticeManagement"
let propsSeq = 
    [
        ".cs"
        ".fs"
    ]
    |> Seq.collect (fun ext -> Directory.GetFiles(rootPath, sprintf "*%s" ext, SearchOption.AllDirectories))
    |> Seq.filter (pathFilter)
    |> Seq.map (fun f -> 
        let code = File.ReadAllText f
        let properties = ModelCollector.VisitMcm Properties code
        let methods = ModelCollector.VisitMcm Methods code
        let mi = ModelCollector.VisitMcm MethodInvocations code
        f, properties, methods, mi 
    )
    
    //.Where(x => x.Properties.Any())
    |> Seq.truncate 6
    |> Dump
    |> ignore
    
