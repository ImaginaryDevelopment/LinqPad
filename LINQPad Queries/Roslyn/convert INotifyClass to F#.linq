<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Text.Encoding.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Threading.Tasks.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <GACReference>System.Collections.Immutable, Version=1.1.33.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <NuGetReference Prerelease="true">Microsoft.CodeAnalysis.CSharp</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

open System
open System.Collections.Generic
open System.IO
#if INTERACTIVE
#r "System.Runtime"
#r "System.Threading.Tasks"
#r "System.Text.Encoding"
#r "System.IO"
#if MONO
#r "./packages/System.Collections.Immutable.1.1.33-beta/lib/portable-net45+win8+wp8+wpa81/System.Collections.Immutable"
#else
#r "System.Collections.Immutable"
#I @"C:\Program Files (x86)\Microsoft Web Tools\DNX\dnx-clr-win-x64.1.0.0-beta4\bin\"
#r @"Microsoft.CodeAnalysis.dll"
#r @"Microsoft.CodeAnalysis.CSharp.dll"
#endif
#endif


    
type System.String with
    static member before (d:string) (s:string) = s.Substring(0, s.IndexOf(d))
    static member after (d:string) (s:string) = s.Substring( s.IndexOf(d) + d.Length)
    static member contains d (s:string) = s.Contains(d)
    static member beforeOrSelf d (s:string) = if s |> String.contains d then s|> String.before d else s
    static member trim (s:string) = s.Trim()
    static member join d (items:string seq) = String.Join(d,items |> Array.ofSeq)
    static member replace (d:string) r (s:string)= s.Replace(d,r)
    static member splitLines (s:string) = s.Split(["\r\n"]|> Array.ofSeq, StringSplitOptions.RemoveEmptyEntries)
    static member optionToStringOrEmpty (s:string option) = match s with Some text -> text | None -> String.Empty
    static member indent spacing (text:string) =
        if String.IsNullOrEmpty(text) then 
            String.Empty 
        else if String.trim text |> String.contains "\r\n" then
            "\r\n" +  text |> String.splitLines |> Seq.map (fun s -> spacing + s) |> String.join "\r\n"  
        else text

let (|Null|Empty|Single|Multiline|) (s:string)=
    match s with 
    | null -> Null
    | _ when String.IsNullOrEmpty(s) -> Empty
    | x when String.trim s |> String.contains "\r\n" -> Multiline
    | _ -> Single

type Identifier = | Identifier of string
type Path = |Path of string
type Code = |Code of (Path option) * string
type CodeSource = | Text of Code | File of Path
type CodeSources = | Single of CodeSource | Directory of Path
type DebugOpt = | Yes | Indent of string | No
type DebugVote = | Promote |Abstain (* questionable value *) | Demote
type DebugDelegate = DebugDelegate of (DebugVote -> (DebugOpt*DebugDelegate))
type PropertyOptions =
    | NoNotify
    | KeepNotify
    | InheritFsharpViewModule
type ClassMember = 
    | Property of string
    | Method of string 
    | Interface of string

    //| Promote -> let newState = DebugOpt.Yes, getDebugOpt
    
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
type TypeSpecification = | Type of Type | Kind of SyntaxKind



//type DebugPredicate = |ByName of (string -> DebugOpt) |ByExpr of (SyntaxNode -> DebugOpt)

        
type ClassDeclaration = { ClassAttributes: string list; Name:string; BaseClass :string option; Fields: string list; Members: ClassMember list} with
    member x.AttributeText() = x.ClassAttributes |> Seq.map (fun ta -> sprintf "[<%s>]" ta) |> String.join "\r\n" 
    member x.FieldText spacing = x.Fields |> Seq.map (fun f -> spacing + f) |> String.join "\r\n"

module Seq =
    #if INTERACTIVE
    let inline iterDump items = items |> Seq.iter (fun i-> printfn "%A" i)
    let inline iterDumpInd indent items = items |> Seq.iter( fun i -> printfn "%s%A" indent i)
    #else
    let inline iterDump items = items.Dump(1)
    let inline iterDumpInd indent items = 
        Util.WithStyle(items,"font-size:large;margin-left:60px").Dump(1)
    #endif
    
    let inline dump items = printfn "%A" items; items
    let inline dumps title items = 
        if Seq.isEmpty items then printfn"%s: is empty" title else printfn "%s:%A" title items
        items

    let inline ofType<'a> (source : System.Collections.IEnumerable) : 'a seq= 
        let resultType = typeof<'a>
        seq {
            for item in source do 
                match item with
                | null -> ()
                | _ -> 
                    if resultType.IsAssignableFrom (item.GetType()) then
                        yield (downcast item)
        }

[<AutoOpen>]
module Helpers =
    let enumerateAllFiles rootPath pattern = System.IO.Directory.EnumerateFiles(rootPath,pattern, SearchOption.AllDirectories)
    let readAllText filePath = File.ReadAllText(filePath)
    let inline flip f arg1 arg2 = f arg2 arg1
    
    // let test = <@ typeof<BlockSyntax> @>
    let typeToString= function
        |FSharp.Quotations.Patterns.Call (e,r,children) -> printfn "Call %A,%A,%A" e (r.GetGenericArguments().[0].Name) children
        |_ as x -> failwithf "call must match typeof<Foo> %A" x

    let inline sprintForDump title o = sprintf "%s:%A" title o
    
    #if INTERACTIVE
    let inline private dumps' title o = printfn "%s" (sprintForDump title o); o
    #else
    let inline private dumps' (s:string) (o:'a) : 'a = o.Dump(s,Nullable(1)); o
    #endif
    let dumps s debugOpt o = 
        match debugOpt with | Yes -> dumps' s o | Indent spc -> dumps' (sprintf "%s%s" spc s) o | No -> o
    let dump title debugOpt o  = 
        dumps title debugOpt o |> ignore
    let dumpf t debugOpt f s = 
        let shouldBeUnit = f s |> dumps t debugOpt
        shouldBeUnit|> ignore // ignore f of s, return s
        s
    let debugLines debug (lines:string seq) = 
        match debug with
        | Yes -> Seq.iterDump lines
        | Indent spc -> Seq.iterDumpInd spc lines
        | No -> ()
    
    // alternate implementation
    //let ofType<'a> (items: _ seq) = items |> Seq.filter(fun x -> box x :? 'a) |> Seq.cast<'a>


module FileWalker = 

    type ModelCollector() = 
        inherit CSharpSyntaxWalker()
        let implementedInterfaces = new Dictionary<string,string list>()
        member private __.ImplementedInterfaces() = implementedInterfaces
        //full of fail:
        // member private x.ImplementedInterfaces = new Dictionary<string,string list>()
        static member VisitClassInterfaces (root:CompilationUnitSyntax) =
            let mc = new ModelCollector()
            mc.Visit(root)
            mc.ImplementedInterfaces()
            //|> Seq.dumps "implemented interfaces"
        override __.VisitBaseList node = 
            let parentIdentifier = (node.Parent :?> ClassDeclarationSyntax).Identifier.ValueText
            let bases = 
                node.Types
                |> Seq.map (fun t-> t.Type)
                |> Seq.ofType<IdentifierNameSyntax>
                |> Seq.map (fun ins -> ins.Identifier.ValueText)
                //|> Seq.dumps (sprintf "bases on %A" parentIdentifier)
                |> List.ofSeq
            implementedInterfaces.Add(parentIdentifier,bases)
            base.VisitBaseList node
    let getSrcCode codeSrc = 
        match codeSrc with
        | Text code -> code
        | File (Path path) -> path |> readAllText |> (fun c -> Code (Some(Path path),c))

    let walkCode code = 
        let src = match code with |Code (p,src) -> src
        let tree = CSharpSyntaxTree.ParseText(src)
        let root = tree.GetRoot() :?> CompilationUnitSyntax

        let classesToBases= 
            let clsToBases = new Dictionary<string,string list>()
            let dic = ModelCollector.VisitClassInterfaces root
            dic
            //|> Seq.dumps "interfaces!"
            |> Seq.filter (fun i -> i.Key <> null && (i.Value |> (* Seq.dumps "bases" |> *) Seq.exists (fun v -> v = "DataModelBase")))
            |> Seq.iter (fun kvp -> clsToBases.Add(kvp.Key, kvp.Value))
            clsToBases
        if classesToBases.Count > 0 then
            Some (code,root,classesToBases)
        else None


let files source = 
    let codeSourceMap cs = cs |> FileWalker.getSrcCode |> FileWalker.walkCode
    match source with
    | Single cs ->
        [ codeSourceMap cs ]
    | Directory (Path dir) -> 
        printfn "rootPath = %s" dir
        enumerateAllFiles dir "*DataModel.cs"
        |> Seq.map Path
        |> Seq.map CodeSource.File
        |> Seq.map codeSourceMap
        |> List.ofSeq

let (|SimpleGetter|_|) (getter: AccessorDeclarationSyntax) =
    let nodes = getter.DescendantNodes() |> Array.ofSeq
    if nodes.Length = 3 && 
        nodes.[0] :? BlockSyntax &&
        nodes.[1] :? ReturnStatementSyntax &&
        nodes.[2] :? IdentifierNameSyntax then
        Some (
            nodes.[0] :?> BlockSyntax, 
            nodes.[1] :?> ReturnStatementSyntax,
            nodes.[2] :?> IdentifierNameSyntax
            )
    else None

type PropertyInfoB = { IsSimpleGet:bool; IsINotify:bool; Type:string; FieldName: string option; PropertyName:string; Getter:AccessorDeclarationSyntax option; Setter:AccessorDeclarationSyntax option}

let mapName s= 
        match String.trim s with
        | "" -> failwithf "name cannot be empty"
        | Id when Id.EndsWith("ID") -> Id |> String.before "ID" |> flip (+) "Id"
        | _ as s -> s
module ScriptOptions = 
    let propertyOptions = PropertyOptions.NoNotify
    let typeAttributes = ["AllowNullLiteral"]
    let searchLinqPadQueriesForSample() =
        let myDocs = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
        Path.Combine(myDocs,"LINQPad Queries","Roslyn")

    #if INTERACTIVE
    let srcPath = 
        let workTarget = @"C:\TFS\Pm-Rewrite\Source-dev-rewrite\PracticeManagement\PracticeManagement.Foundation\DataModels"
        if File.Exists(workTarget) then 
            workTarget (*  Environment.ExpandEnvironmentVariables("%devroot%"); *) 
        else
            //TODO: consider checking devroot next?
            searchLinqPadQueriesForSample()
    #else
    let srcPath = searchLinqPadQueriesForSample()
    #endif
    printfn "searching path %s" srcPath
    let source = CodeSources.Directory (Path srcPath (*  Environment.ExpandEnvironmentVariables("%devroot%"); *) )
    
    let promoteUninitializedStructsToNullable = true
    let includeOriginalInComments = true
    let includeMatchTypeInComments = true
    let selfIdentifier = "x"
    let spacing = "  "
    let isDebugFieldPred name = if name ="_AppointmentEndTime" then Promote else Demote
    let isDebugPropPred name = if name = "LOS" then Promote else Demote
    let isDebugCode code = match code with | Code(p,s) -> Abstain 
    let isDebugClass name =name |> ignore; Abstain
    let isDebugNode (node: #SyntaxNode) = Abstain
    let isDebugNodeResult text= Abstain // if String.contains text "AddMinutes" then Promote else Abstain
    let rec startDebugState state vote : DebugOpt * DebugDelegate =
        let state = 
            match state,vote with
            | Some(x), Abstain -> x
            | None, Abstain -> DebugOpt.No
            | Some (DebugOpt.Yes), Promote -> DebugOpt.Indent spacing
            | Some (DebugOpt.Indent spc), Promote -> DebugOpt.Indent(spc + spacing)
            | Some (DebugOpt.No), Promote -> DebugOpt.Yes
            | Some (DebugOpt.Indent spc), Demote -> if spc.Length > spacing.Length then DebugOpt.Indent (String.before spacing spc) else DebugOpt.Yes
            | _, Demote -> DebugOpt.No
            | _, Promote -> DebugOpt.Yes
            
        let dd = DebugDelegate (fun vote -> startDebugState (Some state) vote)
        state,dd
    let getNextDebugState debugDelegate vote =
        match debugDelegate with |DebugDelegate getDebugOpt -> getDebugOpt vote
    //let private debugIndent spc debug = match debug with |Yes -> Indent spc |Indent indent -> Indent (indent + spc) | No -> No
    
    let childGetDebugOptions (dd: DebugDelegate) (f:unit->DebugVote):DebugOpt*DebugDelegate =
        let myVote =  f()
        match dd with |DebugDelegate f -> f myVote

    let filesToClassesToBases files = 
        files
        |> Seq.choose id
        |> Array.ofSeq
let toFType promoteUninitializedStructsToNullable (t:string) = 
            match String.trim t with
            | nullable when nullable.Contains("?") -> nullable |> String.before "?" |> sprintf "Nullable<%s>"
            | x when x="Guid" || x = "System.Guid" -> if promoteUninitializedStructsToNullable then sprintf"(* NullableWithoutInit*) Nullable<%s>" x else x
            | x when x="Image" -> "System.Drawing.Image"
            | _ as type' -> type'
let toFull (node:#SyntaxNode) = node.ToFullString() |> String.trim
let mapToken (token:SyntaxToken)=
    match token.ValueText with
    |"==" -> "="
    | "!=" -> "<>"
    |"&&"
    | _ as result -> result
let toSyntaxNodeOpt x = match x with | Some n -> n :> SyntaxNode |> Some | None -> None

let rec mapNode promoteUninitializedStructsToNullable spacing (memberNames:Set<string>) (getDebugOpt: DebugDelegate) (node:SyntaxNode) =
    let debugOption, getDebugOpt = ScriptOptions.getNextDebugState getDebugOpt (ScriptOptions.isDebugNode node)
    
    let inline dumps title (o:'a) :'a = dumps title debugOption o
    let inline dump title o : unit = dumps title o |> ignore
    let toFType = toFType promoteUninitializedStructsToNullable
    let printNodeDiagnostics (n: #SyntaxNode) =
        (n.Kind()) |> dump "node diagnostics" 
        let children = n.ChildNodes()
        if Seq.isEmpty children = false then
            dump "children" (children |> Seq.map (fun c -> c.Kind()) |> Array.ofSeq)
        dump "HasTrivia" (n.HasLeadingTrivia, n.HasStructuredTrivia, n.HasTrailingTrivia)
        dump "missing,isstructured" (n.IsMissing, n.IsStructuredTrivia)

    let mapNodeP node = mapNode promoteUninitializedStructsToNullable spacing memberNames getDebugOpt node
    
    let mapChildren delimiter (node:#SyntaxNode) = node.ChildNodes() |> Seq.map mapNodeP |> String.join delimiter
    
    match node with
    | null -> failwithf "null node"
    | :? BlockSyntax as bs -> "BlockSyntax", mapChildren "\r\n" bs
    | :? ElseClauseSyntax as ecs -> "ElseClauseSyntax", sprintf " else %s " (mapNodeP ecs.Statement)
    | :? ReturnStatementSyntax as rss -> "ReturnStatementSyntax", mapChildren  "\r\n" rss
    | :? InvocationExpressionSyntax as ies -> 
        
        let expr = mapNodeP ies.Expression

        if  expr.Contains("AddMinutes") then
            dump "IES:AddMinutes" ies.ArgumentList.Arguments.[0]
        let ignoredResult = expr.StartsWith("builder.Append")
        
        //dump "ies details" <| sprintf "%A" (expr,ies.Expression.GetType().Name, ies.ArgumentList.Arguments, ies.ChildNodes())
        let arguments =ies.ArgumentList.Arguments|> Seq.map mapNodeP |> String.join ","
        "InvocationExpressionSyntax", sprintf "%s(%s)%s" expr arguments (if ignoredResult then "|> ignore" else String.Empty)
    | :? LocalDeclarationStatementSyntax as ldss ->
        let full = if ldss.Declaration.Variables.Count> 0 then Some (mapNodeP ldss.Declaration.Variables.[0]) else None
        match ldss.Declaration.Type.IsVar, ldss.Declaration.Variables.Count, full with
        | true,1, (Some x) ->"ldss var", "let mutable " + x
        | false, 1, (Some x) -> 
            if x |> String.contains "=" then
                "ldss nonVarX", "let mutable " + x
            else
                dump "nonVarUninit" x
                let type' = toFType <| mapNodeP ldss.Declaration.Type 
                "ldss nonVarUninit", sprintf "let mutable %s:%s (* %s *)" x type' "nonVar" + " = null"

        | _ -> 
            dump "ldss" <| sprintf "%A" (ldss.Declaration.Type, ldss.Declaration.Variables.Count, ldss.Declaration.Variables.[0])
            "LocalDeclarationStatementSyntax", ldss |> toFull |> String.replace "var " "let " |> String.trim
    | :? ConditionalExpressionSyntax as ces ->
        "ConditionalExpressionSyntax", sprintf "(if %s then %s else %s )" (mapNodeP ces.Condition) (mapNodeP ces.WhenTrue) (mapNodeP ces.WhenFalse)
    | :? ParenthesizedExpressionSyntax as pes ->
        "ParenthesizedExpressionSyntax", sprintf "(%s)" (mapNodeP pes.Expression)
    | :? ElementAccessExpressionSyntax as eaes ->
        "ElementAccessExpressionSyntax", sprintf "%s.%s" (mapNodeP eaes.Expression) (toFull eaes.ArgumentList)
    | :? LiteralExpressionSyntax as les ->
        dump "parent" les.Parent
        
        match les.Kind() with
        | SyntaxKind.NullLiteralExpression -> 
            if les.Parent :? BinaryExpressionSyntax then
                "NullLiteralExpression(caes)", "null"
            else
                dump "NullLiteralExpression" <| les.Parent.GetType().Name + (les.Parent.Kind().ToString())
                "NullLiteralExpression","(Nullable())"
        | SyntaxKind.NumericLiteralExpression -> 
                
            let ggpFull = toFull les.Parent.Parent.Parent
            let full = toFull les
            let gpk = les.Parent.Parent.Kind()
            if gpk <> SyntaxKind.CaseSwitchLabel && ggpFull |>  String.contains "decimal" && full |> String.contains "." |> not then 
                "NumericLiteralExpression.decimal", (full + "m")
            else 
                //printNodeDiagnostics les
                //dump "NLE ggp" ggpFull
                //dump "NLE gpk" gpk
                //dump "NLE pk" <| les.Parent.Kind()
                //dump "NLE" full

                "NumericLiteralExpression", full 
        | _ -> "NullableTypeSyntax", (toFull les)
    | :? SwitchStatementSyntax as sss ->
        let sections =  "(*switchstatement start*)\r\n" + (Array.ofSeq sss.Sections |> Seq.map mapNodeP |> String.join String.Empty)
        "SwitchStatementSyntax", sprintf "match %s with %s" (mapNodeP sss.Expression) sections

    | :? IfStatementSyntax as ifss -> 
        let statement = mapNodeP ifss.Statement
        let elseblock = if ifss.Else <> null then dumps "found else!" <| mapNodeP ifss.Else |> Some else None
        let elseblock,matchType = match elseblock with Some text -> sprintf " %s" text,"IfElseStatementSyntax" | _ -> String.Empty,"IfStatementSyntax"
        let statements = if statement.Contains("\r\n") then "(*switchstatement multiline*)\r\n" + ScriptOptions.spacing + (String.replace "\r\n" ("\r\n" + ScriptOptions.spacing) statement) + "\r\n" else statement 
        matchType, sprintf "if %s then %s%s" (mapNodeP ifss.Condition) statements elseblock

    | :? PrefixUnaryExpressionSyntax as pues -> "PrefixUnaryExpressionSyntax", sprintf "not <| ( %s )" (mapNodeP pues.Operand) 
    | :? SwitchSectionSyntax as sss ->
            
            let labels,statements = sss.Labels |> Array.ofSeq, sss.Statements |> Array.ofSeq
            let label = labels |> Seq.map mapNodeP |> Array.ofSeq
            let label = String.join String.Empty label
            dump "label" label
            if Seq.isEmpty statements then 
                "SwitchSectionSyntax.nostatements", sprintf "%s" label
            else
                let statement = statements |> Seq.map mapNodeP |> String.join ("(*switch section end*)\r\n") |> String.indent spacing 
                //let statement = ScriptOptions.spacing + statement
                if String.IsNullOrEmpty(statement) then 
                    "SwitchSectionSyntax.labelOnly", sprintf "%s" label
                else if statement |> String.trim |> String.contains "\r\n" then
                    "SwitchSectionSyntax.multiStatement", sprintf "%s ->\r\n%s" label statement
                else
                    "SwitchSectionSyntax", sprintf"%s ->%s" label statement
    | :? DefaultSwitchLabelSyntax as dsls -> "DefaultSwitchLabelSyntax", "|_"
    | :? SwitchLabelSyntax as sls ->
        printNodeDiagnostics sls
        let children = sls.ChildNodes() |> Array.ofSeq
        if children.Length = 1 then
            if children.[0] :? BlockSyntax then
                "SwitchLabelSyntax(bs)", sprintf "|%s" (mapNodeP children.[0])
            else
                dump "SwitchLabelChild" children.[0]
                "SwitchLabelSyntax(1)", sprintf "|%s" (mapNodeP children.[0])
        else
            dump "SwitchLabelChildren" children
            "SwitchLabelSyntax", toFull sls
    | :? AssignmentExpressionSyntax as aes -> "AssignmentExpressionSyntax", sprintf "%s <- %s" (mapNodeP aes.Left) (mapNodeP aes.Right)
    | :? BreakStatementSyntax as bss ->
        "BreakStatementSyntax", String.Empty
    | :? MemberAccessExpressionSyntax as maes -> 
        //dump "maes details" <| sprintf "%A" (maes.Name,maes.Expression, maes.OperatorToken)
        
        let expr = mapNodeP maes.Expression
        let token = mapToken maes.OperatorToken
        let name = mapNodeP maes.Name
        dump "maes details2" <| sprintf "%A" (expr,token,name)
        match expr, token with
        | "this","." -> "This(maes)", sprintf "%s.%s" ScriptOptions.selfIdentifier (mapName name)
        | "string","." -> "string(maes)", (sprintf "%s.%s" "String" name|> dumps "maes result")
        | x, _ when x.Contains("this.") -> "this.(maes)", x|> String.replace "this." "x."
        |_ -> "MemberAccessExpressionSyntax", sprintf "%s%s%s" expr token (mapName name)
    | :? ThisExpressionSyntax as tes -> "ThisExpressionSyntax", "x"
    | :? ArgumentSyntax as arg -> "ArgumentSyntax", mapChildren String.Empty arg 
    | :? BinaryExpressionSyntax as bes -> "BinaryExpressionSyntax", sprintf "%s %s %s" (mapNodeP bes.Left) (mapToken bes.OperatorToken) (mapNodeP bes.Right)
    | :? ExpressionStatementSyntax as ess -> "ExpressionStatementSyntax", mapChildren String.Empty ess
    | :? EqualsValueClauseSyntax as evcs -> 
            "EqualsValueClauseSyntax", sprintf "= %s" (mapNodeP evcs.Value)
    | :? VariableDeclaratorSyntax as vds ->
            //dump "vds " (sprintf "would have been %s" (toFull vds))
            "VariableDeclaratorSyntax", sprintf "%s %s" (mapToken vds.Identifier) (mapChildren String.Empty vds)
    | :? IdentifierNameSyntax as ins -> 
        if ins = null then failwithf "no identifier for ins %s" (toFull node)

        let ident = ins.Identifier
        if ident.ValueText = null then failwithf "no ValueText for ins %s" (toFull ins)
        let value = mapName ins.Identifier.ValueText
        //dump "ins" <| sprintf "(parentType %A, parent %A, isVar %A, arity %A,identifier %A,kind %A)" (ins.Parent.GetType()) ins.Parent ins.IsVar ins.Arity ins.Identifier (ins.Kind())
        if memberNames.Contains(value) && value.Contains(".") = false && ins.Parent :? MemberAccessExpressionSyntax = false then
            "Ins:(propName)", sprintf "x.%s" value
        else
            let name = mapName ins.Identifier.ValueText
            if name.StartsWith("_") then 
                "Ins:(_)", sprintf "%s" name
            else  if ins.Parent :? ArgumentSyntax then  
                "Ins:()", sprintf "x.%s" name
            else
                 "IdentifierNameSyntax", sprintf "%s" (mapName ins.Identifier.ValueText)
    | n when n.Kind() = SyntaxKind.AddExpression ->
        //if printDebug then 
            //printfn "AddExpression is type %s" (n.GetType().Name)
            //n|> dumps "mapNode:AddExpression" |> ignore
        let result = toFull n
        //if printDebug then 
            //result |> dumps "mapNode:AddExpressionText" |> ignore
        "AddExpression", result
    | _ -> "default",node |> toFull
    |> fun (t,o) -> 
        let debugOption,getDebugOpt = ScriptOptions.getNextDebugState getDebugOpt (ScriptOptions.isDebugNodeResult o)
        let dumpResult matchType r = dumpf matchType debugOption (fun r-> (node.GetType().Name + "," + matchType + "," + node.Kind().ToString()) + "=\""+ r.ToString()+"\"") r
        dumpResult (sprintf "%s.%s" t <| node.Kind().ToString()) o

let mapNodeChildren promoteUninitializedStructsToNullable spacing (memberNames:Set<string>) (getDebugOpt:DebugDelegate) delimiter (node:#SyntaxNode) = 
    let debugOption, getDebugOpt = ScriptOptions.getNextDebugState getDebugOpt (ScriptOptions.isDebugNode node)
    let mapNodeC = mapNode promoteUninitializedStructsToNullable spacing memberNames getDebugOpt
    node.ChildNodes() |> Seq.map mapNodeC |> String.join delimiter

let mapPropertyDeclaration (prop:PropertyDeclarationSyntax) =
    let accessorCount = prop.AccessorList.Accessors.Count
    if accessorCount > 2 then failwithf "too many accessors %s" prop.Identifier.ValueText
    let tryFindAccessor k = prop.AccessorList.Accessors |> Seq.tryFind (fun a -> a.Kind() = k)
    let getter = tryFindAccessor SyntaxKind.GetAccessorDeclaration
    let setter = tryFindAccessor SyntaxKind.SetAccessorDeclaration
    let defaultResult = 
        {
            IsSimpleGet=false;
            IsINotify=prop.AccessorList.ToFullString().Contains("SetAndNotify")
            Type = prop.Type.ToFullString()
            PropertyName = prop.Identifier.ToFullString()
            FieldName = None 
            Getter=getter
            Setter=setter
        }

    match getter with
    | Some (SimpleGetter (block,ret,ident)) -> 
        {defaultResult with IsSimpleGet = true;FieldName= if ident.ToFullString().StartsWith("_") then Some <| (mapName (ident.ToFullString())) else None}
    | _ -> defaultResult
    
let getProperties (root:CompilationUnitSyntax) =
    let nodes = root.DescendantNodes() |> Array.ofSeq
    let values = [ 1; 2;3]
    let values = values|> Seq.ofType<int>
    nodes
    |> Seq.map box
    |> Seq.ofType<PropertyDeclarationSyntax> 
    |> Seq.map mapPropertyDeclaration

type FileInfoB = {File:Code; Class':string; Bases: string list;Fields:FieldDeclarationSyntax list; Properties: PropertyInfoB list}
let q() = 
    let files' = files ScriptOptions.source
    let files' = files' |> Seq.choose id |> Array.ofSeq
    let filesToClassesToBases = 
        files' |> Seq.length |> printfn "Checking %i files"   
        files' 
    query{
        for (file,root,clsToBases) in filesToClassesToBases do
        for cls in clsToBases.Keys do
        let bases = clsToBases.[cls]
        
        // (* already done on line 54 *) 
        // where(Seq.contains "DataModelBase" bases)
        let properties = getProperties(root) |> List.ofSeq
        select {FileInfoB.File=file;Class'=cls;Bases =bases;Fields= root.DescendantNodes() |> Seq.ofType<FieldDeclarationSyntax> |> List.ofSeq ;Properties=properties}
    }


let findModel name fileInfoBseq  = 
    fileInfoBseq |> Seq.tryFind(fun fib -> fib.Class' = name ||fib.Class'.StartsWith(name))
module Declarations = 
    let (|EmptyEnumerable|NonEmpty|) (items: _ IEnumerable) =
        if Seq.isEmpty items then EmptyEnumerable else NonEmpty
    let (|SimpleInit|_|) (nodes:SyntaxNode[]) =
        let simpleKinds = [ 
            SyntaxKind.NumericLiteralExpression
            SyntaxKind.StringLiteralExpression
            SyntaxKind.NullLiteralExpression
            SyntaxKind.FalseLiteralExpression
            SyntaxKind.TrueLiteralExpression
            ]
        if nodes.Length = 4 
            && nodes.[0] :? PredefinedTypeSyntax
            && nodes.[1] :? VariableDeclaratorSyntax
            && nodes.[2] :? EqualsValueClauseSyntax
            && simpleKinds |> Seq.contains ( nodes.[3].Kind()) then
                Some (nodes.[0] :?> PredefinedTypeSyntax, nodes.[1] :?> VariableDeclaratorSyntax, nodes.[2] :?> EqualsValueClauseSyntax, nodes.[3] )
            else None
    
    let (|ArrayMatch|_|) (typeSpecifications: TypeSpecification[]) (nodes:SyntaxNode[]) =
        if nodes.Length = typeSpecifications.Length then
            let zipped = Seq.zip typeSpecifications nodes
            if zipped |> Seq.forall(fun (ts,node) -> match ts with |Type t -> t.IsAssignableFrom(node.GetType()) | Kind k-> node.Kind() = k) then
                Some()
            else None
        else
            None
            
    let (|SimplerInit|_|) (nodes:SyntaxNode[]) = 
        let simplerSpecs = [|TypeSpecification.Type(typeof<IdentifierNameSyntax>); TypeSpecification.Type(typeof<VariableDeclaratorSyntax>)  |]
        match nodes with
        | ArrayMatch simplerSpecs -> Some(nodes.[0] :?> IdentifierNameSyntax, nodes.[1] :?> VariableDeclaratorSyntax)
        | _ -> None

    let (|NullableSimplerInit|_|) (nodes:SyntaxNode[]) = 
        if nodes.Length = 3
            && nodes.[0] :? NullableTypeSyntax
            && nodes.[1] :? PredefinedTypeSyntax
            && nodes.[2] :? VariableDeclaratorSyntax then
                Some()
            else None
    let (|NullableSimpleInit|_|) (nodes:SyntaxNode[]) =
        if nodes.Length = 5
            && nodes.[0] :? NullableTypeSyntax
            && nodes.[1] :? IdentifierNameSyntax
            && nodes.[2] :? VariableDeclaratorSyntax
            && nodes.[3] :? EqualsValueClauseSyntax
            && nodes.[4].Kind() = SyntaxKind.NullLiteralExpression
            then Some ()
            else None
    let (|AutoProperty|_|) (getter'setter:AccessorDeclarationSyntax option*AccessorDeclarationSyntax option) =
        let getter,setter = fst getter'setter, snd getter'setter
        match getter,setter with
        | Some g,Some s -> 
            match g.DescendantNodes(),s.DescendantNodes() with
            | EmptyEnumerable,EmptyEnumerable -> Some ()
            | _ -> None
        | _ -> None
module FieldConversion =
    open Declarations 
    type FieldInfoB = {Type:string; Name:string; Initial:string option; Declaration:VariableDeclarationSyntax}
    let convertFileFields promoteUninitializedStructsToNullable spacing (fileInfoB:FileInfoB) (getDebugOpt:DebugDelegate) : string list =
        let cls = fileInfoB
        let inline mapNode memberNames = mapNode promoteUninitializedStructsToNullable memberNames
        let toFType = toFType promoteUninitializedStructsToNullable
        let mapNodeChildren memberNames = mapNodeChildren promoteUninitializedStructsToNullable memberNames
        let debugOption, getDebugOpt = ScriptOptions.getNextDebugState getDebugOpt  (ScriptOptions.isDebugClass cls.Class')
        
        //let typeText = new System.Text.StringBuilder()
        
            //ScriptOptions.getDebugOptions (Some cls.Class') (Some "Field") name expr debugOpt 
        //let debugFile' name expr = debugFile name expr debugOpt.No 
        let fieldNames = 
            cls.Fields 
            |> Seq.map ( fun f ->f.Declaration.Variables |> Seq.map(fun v -> v.Identifier.ValueText)) 
            |> Seq.collect id 
            |> Set.ofSeq
        //Seq.iterDump fieldNames
        
        let fields = 
            let getDebugFieldOptions name = ScriptOptions.getNextDebugState getDebugOpt  (ScriptOptions.isDebugFieldPred name)
            cls.Fields
            //|> Seq.sortBy (fun f-> f.Declaration.Variables.Item)
            |> Seq.map (fun f -> f.Declaration.ToFullString(), f.Declaration)
            |> Seq.map ( fun (fs,vDeclaration) -> 
                
                if vDeclaration.Variables.Count <> 1 then failwithf "too many vars: %s" (toFull vDeclaration)
                let var = vDeclaration.Variables.[0]
                if var = null then failwithf "bad var"
                let fieldname = var.Identifier.ValueText
                if fieldname = null then failwith "bad var name"
                //printfn "mapping field %s" name
                let fieldname = mapName fieldname
                if fieldname = null then failwithf "failed to map name"

                let debugOption, getDebugOpt = getDebugFieldOptions fieldname
                //let getDebugNodeOptions expr= match getDebugOpt with | DebugDelegate getDebugOpt -> getDebugOpt (ScriptOptions.isDebugNode expr)
                let inline mapNode node = mapNode spacing fieldNames getDebugOpt node // (memberNames:Set<string>) (getDebugOpt: DebugDelegate) (node:SyntaxNode)
                let initializer = if var.Initializer <> null then 
                                        let initializer = mapNode var.Initializer
                                        initializer |> Some 
                                    else None
                {
                    Type = toFType <| mapNode vDeclaration.Type
                    Name= fieldname
                    Initial =  initializer
                    Declaration = vDeclaration
                } )
            |> Seq.sortBy (fun f-> f.Name)
            |> Array.ofSeq
        let toFField (memberNames:Set<string>) (getDebugOpt:DebugDelegate) (fieldInfoB:FieldInfoB) = 
            let name,type',initial,vDeclaration = mapName fieldInfoB.Name, fieldInfoB.Type, fieldInfoB.Initial, fieldInfoB.Declaration
            //printfn "starting field %s" name
            let mapNode = mapNode spacing memberNames
            
            let getNodeDebugOptions expr= ScriptOptions.getNextDebugState getDebugOpt (ScriptOptions.isDebugNode expr)
                
            let debugLines expr lines:unit = 
                let debugOpt,_ = ScriptOptions.getNextDebugState getDebugOpt  (ScriptOptions.isDebugNode expr)
                debugLines debugOpt lines //  let debugLines debug (lines:string seq) = 
            //let mapIndent expr :string = mapNode memberNames (ScriptOptions.getNextDebugState (ScriptOptions.isDebugNode expr)) expr
            let fDec init matchType = 

                let comments = 
                    seq {
                        if ScriptOptions.includeMatchTypeInComments then yield matchType
                        if ScriptOptions.includeOriginalInComments then yield sprintf "(%s)" (toFull vDeclaration)
                    } |> Array.ofSeq
                let comments = if Seq.isEmpty comments then String.Empty else String.Join(";",comments) |> sprintf "//%s"
                sprintf "let mutable %s : %s %s%s" name type' init comments
            let eqNullable = "=Nullable()"
            match initial with
            |Some x when x = "string.Empty"|| x= "String.Empty"-> 
                fDec "= System.String.Empty " "(string.Empty-transform)"
            | Some x -> fDec x "mappedInitializer"
            |_ -> 
                let children = vDeclaration.ChildNodes() |> Array.ofSeq
                let inline debugLines node items = debugLines node items
                match vDeclaration.DescendantNodes() |> Array.ofSeq with //descendant nodes was a poor choice, but it's serving some purpose now
                | NullableSimplerInit -> fDec eqNullable  "NullableSimplerInit"
                | NullableSimpleInit -> fDec eqNullable  "NullableSimpleInit"
                | SimplerInit(_,_) -> fDec  (if type'.Contains("Nullable") then eqNullable  else  "=null") "simpler init" //(Some (fun shouldLift -> if shouldLift then "Nullable()" else fDec "null"))
                | SimpleInit (_,_,_, literalKind) -> fDec  ("=" + toFull literalKind) "simple init"
                | nodes when nodes.Length > 3 && (nodes.[0] :? PredefinedTypeSyntax) && (nodes.[1] :? VariableDeclaratorSyntax) && (nodes.[2] :? EqualsValueClauseSyntax) -> 
                    [
                        yield sprintf "predefined fieldtype node %s" name
                        yield! nodes |> Seq.map (fun n -> n.Kind().ToString()) |> Seq.map (fun m -> sprintf "%s" m)
                    ] |> debugLines vDeclaration
                    let node = nodes |> Array.skip(3) |> Seq.head
                    let debugOpt,getDebugOpt = ScriptOptions.getNextDebugState getDebugOpt  (ScriptOptions.isDebugNode node)
                    let mapped = mapNode getDebugOpt node
                    debugLines node [sprintf "mapped:%s" mapped]
                    let result = fDec ("=" + mapped ) "predefinedType init"
                    debugLines node [ sprintf "  %s" result]
                    result
                | _ when children.Length = 2 && (children.[0] :? VariableDeclaratorSyntax) && (children.[1] :? EqualsValueClauseSyntax) ->
                    let vds = children.[0] :?> VariableDeclaratorSyntax
                    let evcs = children.[1] :?> EqualsValueClauseSyntax
                    let right = mapNode getDebugOpt evcs
                    //let right = mapNode memberNames (getDebugIndentedOptions (Some (upcast evcs))) getDebugFieldOptions evcs.Value
                    debugLines evcs.Value [
                        "fieldChildNodes2"
                        right 
                    ]
                    fDec ("=" + right) "fieldChildNodes2"
                | _ -> 
                    debugLines vDeclaration [
                        yield "fieldDefaultsChildNodes"
                        yield! (vDeclaration.ChildNodes() |> Seq.map (fun n -> n.Kind().ToString()) |> Seq.map (fun m -> sprintf "  %s" m) |> List.ofSeq)
                        ]
                    let debugOpt,getDebugOpt = ScriptOptions.getNextDebugState getDebugOpt  (ScriptOptions.isDebugNode vDeclaration)
                    fDec ("=" + (mapNodeChildren spacing memberNames getDebugOpt String.Empty vDeclaration)) "default init" 

        let fields = fields |> Seq.map (toFField fieldNames getDebugOpt) |> List.ofSeq
        //|> Seq.iter(fun f-> typeText.AppendLine(ScriptOptions.spacing + ) |> ignore)
        //typeText.ToString()
        fields

module PropConversion = 
    open Declarations

    let toFProp promoteUninitializedStructsToNullable spacing (propertyNames:Set<string>) (pib:PropertyInfoB) (getDebugOpt:DebugDelegate) = 
        let debugOpt, getDebugOpt = ScriptOptions.getNextDebugState getDebugOpt (ScriptOptions.isDebugPropPred pib.PropertyName)
        let mapNode = mapNode promoteUninitializedStructsToNullable spacing propertyNames

        let fDec getter setter matchType= 
            let spc = ScriptOptions.spacing
            match getter,setter with
            | Some getter, Some setter -> sprintf "member x.%s //%s\r\n%swith get() = %s\r\n%sand set v = %s\r\n" pib.PropertyName matchType spc getter spc setter
            | Some getter, None -> sprintf "member x.%s //%s\r\n%swith get() = %s\r\n" pib.PropertyName matchType spc getter
            | None, Some setter -> sprintf "member x.%s //%s\r\n%swith set v = %s\r\n" pib.PropertyName matchType spc setter
            | None,None -> sprintf "//could not declare property %s" matchType
        let inline simpleSet fieldName = sprintf "%s <- v" fieldName
        let inline iNotifyProp fieldName propName =
            sprintf "%s;x.RaisePropertyChanged(<@ x.%s @>)" (simpleSet fieldName) propName // F# Quotations with INotifyPropertyChanged -> see also http://www.fssnip.net/4Q
        let getDebugPropOptions name = ScriptOptions.getNextDebugState getDebugOpt  (ScriptOptions.isDebugPropPred name)
        let inline debugPropLines name expr lines:unit = 
            let debugOpt, getDebugOpt = getDebugPropOptions(name)

            let debugOpt,getDebugOpt =if Option.isSome expr then ScriptOptions.getNextDebugState getDebugOpt (ScriptOptions.isDebugNode expr.Value) else debugOpt,getDebugOpt
            debugLines debugOpt lines //  let debugLines debug (lines:string seq) = 
        //let mapIndent expr = mapNode memberNames (debugIndented expr) expr
        let mapNode x :string = mapNode getDebugOpt x
        let mapAccessor name type' (nodes:SyntaxNode[]) = 
            
            let dumpNodeKindInfo =
                nodes |> Seq.map(fun n -> n.Kind()) |> (dumps <| sprintf "%s nodes for %s" type' pib.PropertyName <| debugOpt) 
                |> ignore
            if Seq.isEmpty nodes then //autoprop
                failwithf "map %s is not set up for empty nodes" type'
            //let mapped = nodes |> Seq.map mapNode // why are you a warning? the type has been constrainted to be SyntaxNode
            // Script.fsx(615,47): warning FS0064: 
            (* This construct causes code to be less generic than indicated by its type annotations. 
                The type variable implied by the use of a '#', '_' or other type annotation at or near 'Script.fsx(605,46)-(605,57)' 
                has been constrained to be type 'SyntaxNode'. 
            *)
            let mapped = nodes |> Seq.map (fun n -> mapNode (downcast n))

            let mapped = String.join spacing mapped
            let mapped = if mapped.Contains("\r\n") then "\r\n" + mapped else mapped
            dumps "MapAccessorNodesResult" debugOpt mapped

        let mapGetter name = mapAccessor name "getter"
        let mapSetter name = mapAccessor name "setter"

        match pib.IsINotify, pib.IsSimpleGet, pib.FieldName with
        | true,true, Some fieldName -> fDec pib.FieldName (Some (iNotifyProp fieldName pib.PropertyName)) "SimpleINotify"
        | _ -> 
            
            let spacing = spacing + spacing + spacing
            let mapAccessor map childnodes = childnodes |> Seq.toArray |> map pib.PropertyName |> String.replace "\r\n" (sprintf "\r\n%s" spacing) |> Some
            let mapGetter (getter:AccessorDeclarationSyntax) = getter.ChildNodes()|> mapAccessor mapGetter
            let mapSetter (setter:AccessorDeclarationSyntax) = setter.ChildNodes()|> mapAccessor mapSetter
            let value = match pib.Type with | "bool" -> "false" | _ -> "null"
            match pib.Getter,pib.Setter with
            | AutoProperty -> 
                debugPropLines pib.PropertyName None [sprintf "AutoProperty type,value:(%s,%s)" pib.Type value]
                sprintf "%smember val %s : %s = %s with get, set\r\n" spacing pib.PropertyName pib.Type value
            | Some getter, Some setter ->
                let getter = mapGetter getter
                let matchType,setter' = 
                    let full = toFull setter
                    let match' = System.Text.RegularExpressions.Regex.Match(full,@"set\s*{\s*this\.SetAndNotify\(\(\)\s*=>\s*this\.(?<name>\w+),\s*ref\s*_\k<name>,\s*value\);\s*}")
                    if match'.Success then
                        let fieldName = match pib.FieldName with | Some fn -> fn | _ -> ("_" + match'.Groups.[1].Value)
                        let debugPropLines = debugPropLines pib.PropertyName
                        let result = 
                            match ScriptOptions.propertyOptions with 
                            | InheritFsharpViewModule -> iNotifyProp fieldName pib.PropertyName
                            | KeepNotify -> iNotifyProp fieldName pib.PropertyName
                            | NoNotify -> simpleSet fieldName
                        let sSetter = setter :> SyntaxNode |> Some
                        debugPropLines sSetter [
                            fieldName
                            full
                            result
                        ]
                        "mapSetterMatchINotify", Some result
                    else
                        "using existing getter and setter",mapSetter setter
                fDec getter setter' matchType
            | Some getter, None ->
                fDec (mapGetter getter) None "using existing getter"
            | _ -> sprintf "  // could not generate property for %A\r\n" pib.PropertyName

    let convertProperties promoteUninitializedStructsToNullable spacing (fileInfoB:FileInfoB) = 
        let debugOpt,getDebugOpt = ScriptOptions.startDebugState None (ScriptOptions.isDebugCode fileInfoB.File)
        let propNames = fileInfoB.Properties|> Seq.map(fun p -> mapName p.PropertyName) |> Set.ofSeq
        let toFType = toFType promoteUninitializedStructsToNullable
        let props = fileInfoB.Properties |> Seq.map(fun p -> {p with PropertyName=mapName p.PropertyName;Type=toFType p.Type}) |> Seq.sortBy ( fun p -> p.PropertyName) 
        // toFProp (propertyNames:Set<string>) (pib:PropertyInfoB) (getDebugOpt:#SyntaxNode option -> DebugOpt -> DebugOpt) = 
        
        let f (prop:PropertyInfoB) = toFProp promoteUninitializedStructsToNullable spacing propNames prop getDebugOpt
        props |> Seq.map f

    
let convertFile promoteUninitializedStructsToNullable (cls:FileInfoB) = 
    let debugOpt,getDebugOpt = ScriptOptions.startDebugState None (ScriptOptions.isDebugCode cls.File)
    let spacing = ScriptOptions.spacing
    let classD = {
        ClassAttributes = ScriptOptions.typeAttributes
        Name=cls.Class'
        BaseClass= match ScriptOptions.propertyOptions with | PropertyOptions.InheritFsharpViewModule -> Some "  inherit FSharp.ViewModule.ViewModelBase()" |_ -> None
        Fields=  List.empty
        Members = List.empty
        }
    let classD = 
        let buildInterface() : ClassMember = ClassMember.Interface <| sprintf " interface %s\r\n" "System.ComponentModel.INotifyPropertyChanged"
        match ScriptOptions.propertyOptions with | PropertyOptions.KeepNotify -> {classD with Members = buildInterface() :: classD.Members} |_ -> classD

    let classD ={classD with Fields = FieldConversion.convertFileFields promoteUninitializedStructsToNullable spacing  cls getDebugOpt}
    let props = PropConversion.convertProperties promoteUninitializedStructsToNullable spacing cls |> Seq.map (String.indent spacing)
    let text = sprintf "%s\r\ntype %s() = \r\n%s\r\n\r\n" (classD.AttributeText()) cls.Class' (ScriptOptions.spacing + String.optionToStringOrEmpty classD.BaseClass)
    let text = new System.Text.StringBuilder(text)
    text.AppendLine(classD.FieldText spacing).AppendLine(String.join "\r\n" props).ToString()

let convert limit =
    let mutable lines = 0
    let mutable classes = 0
    let converted = 
        seq{
        let items = q()
        let items = if Option.isSome limit then items |> Seq.take limit.Value else items
        //printfn "converting items %i" (Seq.length items)
        
        for cls in items do
            //printfn "Starting conversion %s" cls.Class'
            let text = convertFile ScriptOptions.promoteUninitializedStructsToNullable cls
            let split = text.Split([| "\r\n" |], StringSplitOptions.RemoveEmptyEntries)
            lines <- lines +  (split |> Seq.length)
            classes <- classes + 1
            yield text
        } |> Array.ofSeq
    printfn "converted %i classes, %i lines" classes lines
    converted
let pdm' fileInfos = 
    let pdm = findModel "PatientDataModel" fileInfos
    let pdm' = pdm |> Option.map (convertFile ScriptOptions.promoteUninitializedStructsToNullable)
    pdm'
let apm' fileInfos =
    let apm = findModel "AppointmentDataModel"  fileInfos
    let apm' = apm |> Option.map (convertFile ScriptOptions.promoteUninitializedStructsToNullable)
    apm'
    
#if INTERACTIVE
#r "System.Windows.Forms"
#endif

let getClip text = System.Windows.Forms.Clipboard.GetText()
let setClip text = if text <> null then System.Windows.Forms.Clipboard.SetText(text)

let getClassToClip f = f() |> Option.iter setClip
let pdm () = getClassToClip (fun () -> pdm' (q()))
let apm () = getClassToClip (fun () -> apm' (q()))
let clipAll ()= 
    let converted = convert None 
    if Seq.isEmpty converted then
        printfn "nothing converted"
    else
        converted |> String.join "\r\n\r\n" |> setClip
clipAll()
