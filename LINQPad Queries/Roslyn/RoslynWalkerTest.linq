<Query Kind="Program">
  <NuGetReference>Microsoft.CodeAnalysis.CSharp</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.CodeAnalysis</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp.Syntax</Namespace>
</Query>

void Main()
{
		var text = ModelCollector.testClass;
		var tree = CSharpSyntaxTree.ParseText(text);
		
		var root = (CompilationUnitSyntax) tree.GetRoot();
		
		IDictionary<string,List<string>> interfaces = ModelCollector
			.VisitClassInterfaces(root)
			//.Where(k => k.Value != null && k.Value.Count>1)
			.ToDictionary(kvp=>kvp.Key, kvp=> kvp.Value);
		interfaces.Dump();
	
}


class ModelCollector : CSharpSyntaxWalker
{
	public const string testClass = @"class ObjectCreationMethodInfo:IComponent {     	public string Type{get;set;} public string MethodName {get;set;} public bool CallsCreate{get;set;} }";
	private ModelCollector() : base()
	{

	}

	public readonly Dictionary<string, List<string>> implementedInterfaces = new Dictionary<string, List<string>>();

	public static IDictionary<string,List<string>> VisitClassInterfaces(CompilationUnitSyntax root){
			var mc = new ModelCollector();
			
			mc.Visit(root);
			return mc.implementedInterfaces;
		}

		public override void VisitBaseList(BaseListSyntax node){
			var parentIdentifier =((ClassDeclarationSyntax) node.Parent).Identifier.ValueText;
			var bases = node.Types
				.Select(t=>t.Type)
				.OfType<IdentifierNameSyntax>()
				.Select(ins => ins.Identifier.ValueText)
				.ToList();
			implementedInterfaces[parentIdentifier] = bases;
			base.VisitBaseList(node);
		}
}