<Query Kind="Program">
  <NuGetReference>Microsoft.CodeAnalysis</NuGetReference>
  <NuGetReference>Microsoft.CodeAnalysis.CSharp</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.CodeAnalysis</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp.Syntax</Namespace>
</Query>

// this appears to walk all files in a solution, and find methods that have a try inside the method

void Main()
{
    //with help from https://github.com/dotnet/roslyn/wiki/Getting-Started-C%23-Semantic-Analysis
    //translate to F# so that we can choose all files under directory, or all files in all projects in a solution
    
    var target= System.Environment.GetEnvironmentVariable("devroot") + @"\PracticeManagement\dev\PracticeManagement\";
    
    ReadSln(target+"PracticeManagementRW_local.sln");
    Directory.GetFiles(target, "*.cs", SearchOption.AllDirectories).Where(fp => !fp.EndsWith(".g.cs") && !fp.EndsWith(".g.i.cs") && !fp.Contains("\\obj\\")).ToList().Dump("files").SelectMany(WalkFile).Dump();
    // if we decide to have the file/method auto open on click in the output this is the hook
    // var devEnvs = System.Diagnostics.Process.GetProcessesByName("devenv"); //.Dump();
}

// Define other methods and classes here
IEnumerable<object> WalkFile(string filePath)
{
    SyntaxTree tree = CSharpSyntaxTree.ParseText(File.ReadAllText(filePath));

    var root = (CompilationUnitSyntax)tree.GetRoot();


    var methods = root.DescendantNodes()
                               .OfType<MethodDeclarationSyntax>()
                               .ToArray();
    if (methods == null || !methods.Any())
        yield break;
    var tryMethods =
        methods
            .Where(m =>m.Modifiers.Any(mo => mo.Text != "override") && m.DescendantNodes().OfType<TryStatementSyntax>().Any())
            .Select(m => new {Identifier = m.Identifier.Text,Modifiers=m.Modifiers, Body= m.GetText().ToString()  })
            .ToArray();
    if (!tryMethods.Any())
        yield break;
    yield return new { File = filePath, TryMethods = tryMethods};

}
void ReadSln(string slnPath)
{
    var slnText = System.IO.File.ReadAllLines(slnPath);
    var projects =
        slnText.SkipWhile(t => t.TrimStart().StartsWith("Project(\"{") == false).TakeWhile(t => t.StartsWith("Global") == false)
        .Where(t => t.TrimStart().StartsWith("Project(\"{"))
        .Select(t => t.After("Project(\""))
        .Select(t => new { SlnFolderGuid = Guid.Parse(t.Before("\"")), Remainder = t.After("\"") })
        .Select(t => new { t.SlnFolderGuid, Remainder = t.Remainder.After("\"") })
        .Select(t => new { t.SlnFolderGuid, Name = t.Remainder.Before("\""), Remainder = t.Remainder.After("\"") })
        .Where(t => t.Name != "Solution Items")
        .Select(t => new { t.SlnFolderGuid, t.Name, ProjectFilePath = t.Remainder.After("\"").Before("\""), Remainder = t.Remainder.After("\"").After("\"") })
        .Select(t => new { t.SlnFolderGuid, t.Name, t.ProjectFilePath, ProjectGuid = Guid.Parse(t.Remainder.After("\"").Before("\"")) })
        .ToDictionary(t => t.ProjectGuid)
        ;
        projects.Dump();

}

void DoCompilation(SyntaxTree tree)
{
    var root = (CompilationUnitSyntax)tree.GetRoot();
    var compilation = CSharpCompilation.Create("HelloWorld")
                                         .AddReferences(
                                              MetadataReference.CreateFromFile(
                                                  typeof(object).Assembly.Location))
                                         .AddSyntaxTrees(tree);
    var model = compilation.GetSemanticModel(tree);
    var nameInfo = model.GetSymbolInfo(root.Usings[0].Name);
    var systemSymbol = (INamespaceSymbol)nameInfo.Symbol;
    var members = systemSymbol.GetNamespaceMembers().Select(s => s.Name);
}