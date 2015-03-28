<Query Kind="Program">
  <NuGetReference Prerelease="true">Microsoft.CodeAnalysis</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.CodeAnalysis</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp.Syntax</Namespace>
</Query>

void Main()
{
	var rootPath = Environment.ExpandEnvironmentVariables("%devroot%");
	rootPath.Dump();
	var code = File.ReadAllText(rootPath+@"\Xpress.Foundation\DataModels\CardiacArrestDataModel.cs");
	ModelCollector.VisitProperties(code).Dump();
}

// Define other methods and classes here
class ModelCollector : CSharpSyntaxWalker
    {
        public readonly Dictionary<string, List<string>> models = new Dictionary<string, List<string>>();
		
		public static IDictionary<string,List<string>> VisitProperties(string text){
			var tree = CSharpSyntaxTree.ParseText(text);
	
			var root = (CompilationUnitSyntax) tree.GetRoot();
			var mc = new ModelCollector();
			mc.Visit(root);
			return mc.models;
		}
		
        public override void VisitPropertyDeclaration(PropertyDeclarationSyntax node)
        {
            var classnode = node.Parent as ClassDeclarationSyntax;
            if (!models.ContainsKey(classnode.Identifier.ValueText))
                models.Add(classnode.Identifier.ValueText, new List<string>());
				
			/* character index in file:
			 	node.FullSpan.Start.Dump(); */
			
            models[classnode.Identifier.ValueText].Add(node.Identifier.ValueText);
        }
		/* since we get no intellisense for override declarations */
		public override void VisitMethodDeclaration(MethodDeclarationSyntax node){
			base.VisitMethodDeclaration(node);
		}
		public override void VisitClassDeclaration(ClassDeclarationSyntax node){
			base.VisitClassDeclaration(node);
		}
    }