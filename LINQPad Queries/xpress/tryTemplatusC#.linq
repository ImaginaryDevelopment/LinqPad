<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\ISymWrapper.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\MSBuild\14.0\Bin\Microsoft.Build.Framework.dll</Reference>
  <Reference>C:\Users\Brandon\.nuget\packages\Templatus\0.2.1\tools\Templatus.exe</Reference>
  <Namespace>Microsoft.FSharp.Compiler.Interactive</Namespace>
</Query>

void Main()
{
    //System.Reflection.Assembly.GetEntryAssembly().
    
    // force load of compiler service 
    
    var x = (Microsoft.FSharp.Compiler.Ast.ExprAtomicFlag.Atomic).Dump();

    try
    {
        Environment.CurrentDirectory = @"C:\projects\Templatus\bin";
        Templatus.Main.main(new[] { "-t", @"..\tests\testTemplate.ttus", "-t", @"..\tests\test Template2.ttus", "-p", "age=2,name=Timmy", "-parallelization", "2"}).Dump("finished one or two!");
    }
    catch (MissingMethodException ex)
    {
        ex.Dump("caught");
        }
}

// Define other methods and classes here