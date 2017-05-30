<Query Kind="FSharpExpression">
  <Reference>C:\Users\Brandon\.vscode\extensions\Ionide.ionide-fsharp-2.25.12\bin\FSharp.Compiler.Service.dll</Reference>
  <Reference>C:\Users\Brandon\.vscode\extensions\Ionide.ionide-fsharp-2.25.12\bin\FSharp.Compiler.Service.ProjectCrackerTool.exe</Reference>
  <Reference>C:\Users\Brandon\.vscode\extensions\Ionide.ionide-fsharp-2.25.12\bin\FSharpLint.Core.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <Namespace>FSharpLint</Namespace>
  <Namespace>FSharpLint.Framework.Analyser</Namespace>
  <Namespace>FSharpLint.Rules.NameConventions</Namespace>
</Query>

let config = 
    "C:\projects\FableInterop\Settings.FSharpLint"
    |> File.ReadAllText
    |> FSharpLint.Application.ConfigurationManagement.loadConfigurationFile
    |> FSharpLint.Application.ConfigurationManagement.overrideDefaultConfiguration
    
printfn "config loaded"
//config.Dump()

// https://github.com/fsprojects/FSharpLint/blob/fcdcaabeb2cef58dface475db1c32739f52a9484/tests/FSharpLint.Core.Tests/Framework/TestAnalyser.fs

//try
//    let ai = {
//                        FSharpVersion= Version()
//                        Config = config
//                        Suggest = (fun x -> x.Dump())
//                        Text=File.ReadAllText @"C:\projects\FableInterop\src\App.fs"}
//    
//    Rules.NameConventions.analyser
//        {
//                Info=ai
//                CheckFile=None
//                SyntaxArray=null
//                SkipArray = null
//        }
//with | :? Framework.Configuration.ConfigurationException as ce ->
//    printfn "%s" ce.Data0
//    ce.Dump()


// on this project, this fails to parse, saying Microsoft.Build.Exceptions.InvalidProjectFileException: The default XML namespace of the project must be the MSBuild XML namespace. If the project is authored in the MSBuild 2003 format
let lintProject() = 
    let targetProj = @"C:\projects\FableInterop\FableInterop.fsproj"
    let setup() = 
        let binPath = @"C:\Users\Brandon\.vscode\extensions\Ionide.ionide-fsharp-2.25.12\bin"
        let tool = "FSharp.Compiler.Service.ProjectCrackerTool.exe"
        let assmPath = System.Reflection.Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
        let shadowPath = Directory.GetDirectories(assmPath) |> Seq.head
        Directory.GetFiles(binPath)
        |> Seq.filter(fun p ->
            Path.Combine(shadowPath, Path.GetFileName p)
            |> fun x -> x.Dump(); x
            |> File.Exists
            |> not
            
        )
        |> Seq.iter(fun p ->
            let target = Path.Combine(shadowPath,Path.GetFileName p)
            printfn "Copying %s to %s" p target
            File.Copy(p,target)
        )
        
        Microsoft.FSharp.Compiler.SourceCodeServices.ProjectCrackerTool.Program.main |> ignore
    setup()
    printfn "Setup finished"
    FSharpLint.Application.Lint.lintProject
        {   CancellationToken=None;Configuration = Some config
            ReceivedWarning=Some (fun x -> x.Dump())}
        targetProj
        (Some(fun pp -> pp.Dump()))
        
let warnings, processWarning = 
    let warnings = ResizeArray<_>()
    let f (w:FSharpLint.Application.LintWarning.Warning) =
        warnings.Add (w.Info, w.Range)
    warnings, f
    
FSharpLint.Application.Lint.lintFile {  CancellationToken=None
                                        Configuration = Some config
                                        ReceivedWarning=Some processWarning}
<| @"C:\projects\FableInterop\src\App.fs"
<| Version()
|> fun result -> 
    Util.ClearResults()
    warnings.Dump()
    Util.OnDemand("huge result", fun () -> result)