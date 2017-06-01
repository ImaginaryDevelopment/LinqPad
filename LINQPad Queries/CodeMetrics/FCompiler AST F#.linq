<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Compiler.Service</NuGetReference>
  <Namespace>Microsoft.FSharp.Compiler</Namespace>
  <Namespace>Microsoft.FSharp.Compiler.Ast</Namespace>
</Query>

open Microsoft.FSharp.Compiler.SourceCodeServices
let checker = FSharpChecker.Create()
let dumpIfListItems title (o:obj) = 
    function
    | [] -> ()
    | x -> x.Dump(sprintf "%s %A" title o)
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
type FunctionAst = {Name:string; Branches: obj list}
type ClassAst = |ClassAst of name:string * FunctionAst list
type ModuleAstItem = | ClassAst of ClassAst | Function
type ModuleAst = 
    |ModuleAst of name:string * ModuleAstItem list
type AstItem = 
    | Class of name:string * FunctionAst list
    | Module of name:string * AstItem list
    
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
            (fp,pp, synModuleOrNamespaces)
            |> Some
    )
    |> Seq.choose id
    |> Seq.map (fun (pp,fp,mons) ->
        mons
        |> Seq.map(fun mON ->
            
            match mON with
            | Microsoft.FSharp.Compiler.Ast.SynModuleOrNamespace(identifiers,isRecursive,isModule, declarations, _xmlDoc, attribs, _accessibility, _range) ->
                let identifier = identifiers |> Seq.map(fun x -> x.idText) |> delimit "."
                    
                identifier,isModule,declarations |> Seq.map(
                    function
                    | SynModuleDecl.ModuleAbbrev(ident,longId,_range) -> box (ident,longId,_range)
                    | SynModuleDecl.NestedModule(synComponentInfo,isRecursive, declarations, _someBool, _range) ->
                        box(synComponentInfo,isRecursive, declarations, _someBool)
                    | x -> box x
                    
                )

        )
    )
    |> Dump
    