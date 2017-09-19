<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Compiler.Service</NuGetReference>
  <Namespace>Microsoft.FSharp.Compiler</Namespace>
  <Namespace>Microsoft.FSharp.Compiler.Ast</Namespace>
</Query>

open Microsoft.FSharp.Compiler.SourceCodeServices

let checker = FSharpChecker.Create()
module List = 
    let toOption x = 
        match x with
        | [] -> None
        | x -> Some x
let dumpIfListItems title (o:obj) = 
    List.toOption
    >> Option.iter (fun x -> x.Dump(sprintf "%s %A" title o))

let pathFilter (path:string) = 
    not <| path.Contains "CodedUITest"
    && not <| path.Contains "ViewModelsC"
    && not <| path.Contains "Pm.TestsC"
    && not <| path.Contains "Packages"
    && not <| path.Contains "node_modules"
    && not <| path.EndsWith "AssemblyInfo.fs"
    && not <| path.Contains "BReusablePm"
let findFsProjParent filePath = 
    let rec f dirPath = 
        Directory.GetFiles(dirPath,"*.fsproj")
        |> Seq.tryHead
        |> function
            | Some fsProjPath -> Some fsProjPath
            | None -> 
                Path.GetDirectoryName dirPath
                |> f
    
    Path.GetDirectoryName filePath
    |> f

    //"C:\TFS\PracticeManagement\dev\PracticeManagement\PracticeManagementRW_local.sln"
let rootPath = @"C:\TFS\PracticeManagement\dev\PracticeManagement"
module AstMapping = 
    let dumpInfo titling includeData (x:obj) (r:Range.range) = 
        let dumpWithType x2 = 
            x2.Dump(sprintf "%s:%s" titling <| x.GetType().Name)
        match r.StartLine = r.EndLine, includeData with
        | true, true ->
            box (r.FileName, r.StartLine, x |> string)
            
        | false, true ->
            box (r.FileName, r.StartLine, r.EndLine, x |> string)
        | true, false -> 
            box (r.FileName, r.StartLine)
        | false, false ->
            box (r.FileName,r.StartLine, r.EndLine)
        |> dumpWithType
        ()
//    type FunctionAst = {Name:string; Branches: obj list}
//    type ClassAst = |ClassAst of name:string * FunctionAst list
//    type ModuleAstItem = | ClassAst of ClassAst | Function
//    type ModuleAst = 
//        |ModuleAst of name:string * ModuleAstItem list
//    type AstItem = 
//        | Class of name:string * FunctionAst list
//        | Module of name:string * AstItem list
    type MethodWrapper = {Namespacing:string; SynType:string; Name:string; Lines:string; Tokens: SynExpr list}
    let makeLinesDisplay s e = 
        if s = e then sprintf "%i" s else sprintf "%i-%i" s e
    let getSynBinding =
        function
        | SynBinding.Binding(_,_,_,_,_,_,_,SynPat.Named(synPat, ident, isSelfIdentifier, _,_),_,expr,range,_) as decl ->
            let lines  = makeLinesDisplay range.StartLine range.EndLine
            ident.idText, lines, [expr]
        | SynBinding.Binding(_,_,_,_,_,_,_,SynPat.LongIdent(liwd,identOpt, _, _, _, _),_,expr,range,_) as decl ->
            let lines  = makeLinesDisplay range.StartLine range.EndLine
            liwd.Lid |> string, lines, [expr]
        
        | SynBinding.Binding(_,_,_,_,_,_,_,synPat,_,expr,range,_) as decl ->
            let lines  = makeLinesDisplay range.StartLine range.EndLine
            (synPat,expr).Dump(lines)
            failwithf "Unexpected synBinding"
    let getSynMemberDefn = 
        function
        | SynMemberDefn.Member(synBinding,range) ->
            getSynBinding synBinding
        | x ->
            dumpInfo "SynMemberDefn2" true x x.Range
            failwithf" Unknown synMember defn"
            
    let getTypeMembers : _ -> MethodWrapper list = 
        function
//        |TypeDefn(ComponentInfo _,SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.TypeAbbrev _,_),[],_) -> None
        |TypeDefn(ComponentInfo _, _,[],_) -> List.Empty
        |TypeDefn(ComponentInfo(attribs,typeParams,contrains,longIdent,xmlDoc,preferPostfix,accessibility,_cRange),typeDefnRepr, members,_range) as x ->
            // we need to find the type name for namespacing the methods found
            match typeDefnRepr with
            // type augmentation
            | SynTypeDefnRepr.ObjectModel(TyconAugmentation,[],typeRange) ->
                let name = longIdent |> Seq.map (fun l -> l.idText) |> delimit "."
                members |> List.map(fun x -> 
                    let (name, lines, tokens) = getSynMemberDefn x
                    
                    { Namespacing = name; SynType="SynTypeDefnRepr.ObjectModel";Name=name; Lines = lines; Tokens=tokens})
                //Some [name,members |> List.map(fun decl ->  decl.GetType().Name, decl.Range.StartLine, decl.Range.EndLine) ]
            | SynTypeDefnRepr.ObjectModel(TyconAugmentation,synMemberDefns,typeRange) ->
                dumpInfo "ObjectModelAug" true x x.Range
                (members |> List.map getSynMemberDefn,synMemberDefns).Dump("type aug members, synmembers")
                List.Empty
            // can contain DU (which can have methods, but those don't appear to be included here?)
            | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(_,cases,range2),range) ->
                List.Empty
            // record can have members attached, but those don't appear to be included here
            | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record _, _ ) ->
                List.Empty
            | x -> 
                dumpInfo "Other DefnRepr" true x x.Range
                typeDefnRepr.Dump("Unknown typeDefn")
                List.Empty
    let (|TypesMembers|) x =
        x
        |> List.map getTypeMembers
        |> List.concat
    
    
    
    let rec findFunctions parentName x : MethodWrapper list= 
        
        match x with
    //    | SynModuleDecl.NestedModule(SynComponentInfo.ComponentInfo(synAttribs,synTyparDecls, synTypeContraints, longId,preXmlDoc,preferPostfix, accessibility,componentRange) as componEnt,isRecursive,synModuleDecls,_someBool, range) ->
        | SynModuleDecl.Let(_someBool, synBindings, range) ->
            
            synBindings |> List.map (getSynBinding >> (fun (name,lines,tokens) -> 
                {Namespacing = parentName; SynType = "LetDecl"; Name=name; Lines=lines; Tokens = tokens}))
            
        | SynModuleDecl.NestedModule(SynComponentInfo.ComponentInfo(_,_,_, longId,_,_,_,_) as componEnt,isRecursive,synModuleDecls,_someBool, range) ->
        
            let name = longId |> Seq.map (fun l -> l.idText) |> delimit "."
            synModuleDecls |> List.map (findFunctions name) |> List.concat
//            synModuleDecls
//            |
//            [ sprintf "NestedModule %s" name, synModuleDecls |> List.map (fun decl -> decl.GetType().Name, decl.Range.StartLine, decl.Range.EndLine)]

        | SynModuleDecl.Types (TypesMembers members,range) -> 
            members 
        | SynModuleDecl.Open _ -> List.Empty
        | _ -> 
            printfn "Unknown SynModuleDecl %s" <| x.GetType().Name
            if x.Range.StartLine <> x.Range.EndLine then
                (x.Range.FileName, x.Range.StartLine, x.Range.EndLine).Dump(sprintf "Unknown SynModuleDecl %s" <| x.GetType().Name)
            else
                (x |> string).Dump(sprintf "Unknown SynModuleDecl %i: %s" x.Range.StartLine (x.GetType().Name))
            List.Empty
()
    
    
let propsSeq = 
    [
        ".fs"
    ]
    |> Seq.collect (fun ext -> Directory.GetFiles(rootPath, sprintf "*%s" ext, SearchOption.AllDirectories))
    |> Seq.filter (pathFilter)
    |> Seq.truncate 3
    |> Seq.map (fun filePath ->
        
        // get the project this file belongs to
        let projectPath = 
            findFsProjParent filePath
            |> function
                | Some pp -> pp
                | None -> failwithf "Could not get project path for %s" filePath
        let dumpIfListItems' x = dumpIfListItems x (projectPath,filePath)    
        let projOptions,errors = 
            checker.GetProjectOptionsFromScript(projectPath, File.ReadAllText projectPath) 
            |> Async.RunSynchronously
        errors |> dumpIfListItems' "projOptionErrors"

        let result = 
            checker.ParseFileInProject(filePath,File.ReadAllText filePath, projOptions)
            |> Async.RunSynchronously
        
        result.Errors |> List.ofSeq |> dumpIfListItems' "ParseErrors"
        result.ParseTree
        |> Option.map (fun pt ->
            projectPath, filePath, pt
        )
    )
    |> Seq.choose id
    |> Seq.map(fun (pp,fp,tree) ->
        let dumpIfListItems' x = dumpIfListItems x (pp,fp)
        match tree with
        |Ast.ParsedInput.SigFile(sigFile) ->
            sigFile.Dump("Sihg fiel!")
            None
        |ParsedInput.ImplFile(implFile) ->            
            let (ParsedImplFileInput(_fileName, _isScript, _qualifiedName, scopedPragmas, parsedHashDirectives, synModuleOrNamespaces,(_someBool1,_someBool2))) = implFile
            scopedPragmas |> dumpIfListItems' "scoped pragmas" 
            parsedHashDirectives |> dumpIfListItems' "parsedHashDirectives"
            (pp,fp, synModuleOrNamespaces)
            |> Some
    )
    |> Seq.choose id
    |> Seq.map (fun (pp,fp,mons) ->
        mons
        |> Seq.map(fun mON ->
            
            match mON with
            | Microsoft.FSharp.Compiler.Ast.SynModuleOrNamespace(identifiers,isRecursive,isModule, declarations, _xmlDoc, attribs, _accessibility, _range) ->
                
                let identifier = identifiers |> Seq.map(fun x -> x.idText) |> delimit "."
                let mw = 
                    declarations 
                    |> Seq.map(AstMapping.findFunctions identifier) 
                    |> Seq.concat
                    //|> Seq.groupBy(fun mw -> mw.Namespacing)
                identifier,(*isModule, *) mw
                
        )
    )
    |> Dump