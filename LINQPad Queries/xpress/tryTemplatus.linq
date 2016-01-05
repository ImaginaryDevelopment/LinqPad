<Query Kind="FSharpProgram">
  <Reference Relative="..\..\..\..\Templatus\bin\Chessie.dll">C:\projects\Templatus\bin\Chessie.dll</Reference>
  <Reference Relative="..\..\..\..\Templatus\bin\FParsec.dll">C:\projects\Templatus\bin\FParsec.dll</Reference>
  <Reference Relative="..\..\..\..\Templatus\bin\FParsecCS.dll">C:\projects\Templatus\bin\FParsecCS.dll</Reference>
  <Reference Relative="..\..\..\..\Templatus\bin\FSharp.Compiler.Service.dll">C:\projects\Templatus\bin\FSharp.Compiler.Service.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\ISymWrapper.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\MSBuild\14.0\Bin\Microsoft.Build.Framework.dll</Reference>
  <Reference Relative="..\..\..\..\Templatus\bin\Templatus.Core.dll">C:\projects\Templatus\bin\Templatus.Core.dll</Reference>
  <Reference Relative="..\..\..\..\Templatus\bin\Templatus.exe">C:\projects\Templatus\bin\Templatus.exe</Reference>
  <Namespace>Microsoft.FSharp.Compiler.Interactive</Namespace>
</Query>

//System.Reflection.Assembly.GetEntryAssembly().
Templatus.Core.Utils.broadcastResolves ()
Templatus.Core.Utils.broadcastLoads ()
Templatus.Core.Utils.redirectAssembly "FSharp.Core" (Version("4.4.0.0")) "b03f5f7f11d50a3a"
    
    
// force load of compiler service 
do 
    Microsoft.FSharp.Compiler.Ast.Bool true
    |> (printfn "%O")

try
    Environment.CurrentDirectory <- @"C:\projects\Templatus\bin"
    Templatus.Main.main([| "-t";@"..\tests\testTemplate.ttus"; "-t"; @"..\tests\test Template2.ttus" ; "-p"; "age=2;name=Timmy"; "-parallelization"; "2" |]).Dump("finished one or two!")
    
    Environment.CurrentDirectory <- @"c:\TFS\PracticeManagement\dev\PracticeManagement-branch\Pm.Schema"
    Templatus.Main.main([| "-t";@"C:\TFS\PracticeManagement\dev\PracticeManagement-branch\Pm.Schema\Template.ttus"; "-p"; "name=Timmy;Age=3" |]).Dump("finished Schema one!")
with |ex -> 
    printfn "Caught exception %A" ex
    ex.Dump("caught")

AppDomain.Unload(AppDomain.CurrentDomain)