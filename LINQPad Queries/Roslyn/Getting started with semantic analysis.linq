<Query Kind="Statements">
  <NuGetReference>Microsoft.CodeAnalysis</NuGetReference>
  <NuGetReference>Microsoft.CodeAnalysis.CSharp</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.CodeAnalysis</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp.Syntax</Namespace>
</Query>

//https://github.com/dotnet/roslyn/wiki/Getting-Started-C%23-Semantic-Analysis
SyntaxTree tree = CSharpSyntaxTree.ParseText(
@"using System;
using System.Collections.Generic;
using System.Text;

namespace HelloWorld
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine(""Hello, World!"");
        }
    }
}");

var root = (CompilationUnitSyntax)tree.GetRoot();
var compilation = CSharpCompilation.Create("HelloWorld")
                                  .AddReferences(
                                       MetadataReference.CreateFromFile(
                                           typeof(object).Assembly.Location))
                                  .AddSyntaxTrees(tree);
var model = compilation.GetSemanticModel(tree);
var nameInfo = model.GetSymbolInfo(root.Usings[0].Name);
var systemSymbol = (INamespaceSymbol)nameInfo.Symbol;
systemSymbol.GetNamespaceMembers().Select(s => s.Name).Dump();

var helloWorldString = root.DescendantNodes()
                           .OfType<LiteralExpressionSyntax>()
                           .First();
var literalInfo = model.GetTypeInfo(helloWorldString);
var stringTypeSymbol = (INamedTypeSymbol)literalInfo.Type;

(
    from method in stringTypeSymbol
        .GetMembers()
        .OfType<IMethodSymbol>()
    where method.ReturnType.Equals(stringTypeSymbol) &&
        method.DeclaredAccessibility == Accessibility.Public
    select method.Name
)
.Distinct()
.Dump();

