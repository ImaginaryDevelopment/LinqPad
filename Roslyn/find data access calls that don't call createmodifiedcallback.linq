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
	var files = Directory.EnumerateFiles(rootPath,"*DataModel.cs", SearchOption.AllDirectories).Skip(0)//.Take(20)
		;
	var fileToClassesToInterfaces = new Dictionary<string,IDictionary<string,List<string>>>();
	foreach(var file in files){
		// if data model implements ICanAudit, make sure all constructor calls in the DataAccessModel are followed by a call to CreateCallback
		var text = File.ReadAllText(file);
		var tree = CSharpSyntaxTree.ParseText(text);
		
		var root = (CompilationUnitSyntax) tree.GetRoot();
		
		IDictionary<string,List<string>> interfaces = ModelCollector
			.VisitClassInterfaces(root)
			.Where(k => k.Value != null && k.Value.Count>1)
			.ToDictionary(kvp=>kvp.Key, kvp=> kvp.Value);
		if(interfaces.Any()){
			
			fileToClassesToInterfaces[file]= interfaces;
		}
	}
	
	var q= 
		from c in fileToClassesToInterfaces.Keys
		from cls in fileToClassesToInterfaces[c].Keys
		let bases = fileToClassesToInterfaces[c][cls]
		where bases.Contains("ICanCreateModificationsCallback")
		let targetFilename = cls.Before("DataModel") + "DataAccess.cs"
		let dataAccess = FindFile(rootPath,targetFilename)
		let dummy = targetFilename.DumpIf(s=> dataAccess==null,"no data access model found")
		where dataAccess!= null
		let text = File.ReadAllText(dataAccess)
		let tree = CSharpSyntaxTree.ParseText(text)
		let root = (CompilationUnitSyntax) tree.GetRoot()
		select new{ c, cls, /* bases, */ targetFilename, CreationMethods = GetObjectCreationsOfType(root,cls)};
	q.OrderByDescending(q1 => q1.CreationMethods
		.Any(cm => cm.CallsCreate == false))
		.Select(q1=> new{ q1.c, q1.cls,q1.targetFilename, CreationMethods= 
			q1.CreationMethods.Select(cm => 
				new{ cm.Type,cm.MethodName, CallsCreate = Util.HighlightIf(cm.CallsCreate,cc=> !cc)}
			)}).Dump();
}

static string FindFile(string rootPath, string targetFilename){
	var file = Directory.EnumerateFiles(rootPath,targetFilename, SearchOption.AllDirectories).FirstOrDefault();
	return file;
}

static bool CallsCreateModifiedCallback(IEnumerable<SyntaxNode> methodNodes){
	return methodNodes
		.OfType<InvocationExpressionSyntax>()
		.Select(s=> s.Expression)
		.OfType<MemberAccessExpressionSyntax>()
		.Select(s=> s.Name)
		.OfType<IdentifierNameSyntax>()
		.Any(s=> s.Identifier.ValueText =="CreateModifiedCallback");
}

class ObjectCreationMethodInfo {
	public string Type{get;set;}
	public string MethodName {get;set;}
	public bool CallsCreate{get;set;}
}
 static IEnumerable<ObjectCreationMethodInfo> GetObjectCreationsOfType(CompilationUnitSyntax node, string name){
	var methodsThatCreateSpecifiedType = from m in node.DescendantNodes().OfType<MethodDeclarationSyntax>()
			
			from ctor in m.DescendantNodes().OfType<ObjectCreationExpressionSyntax>()
			let type = ctor.Type as IdentifierNameSyntax
			where type != null
			let typeName = type.Identifier.ValueText
			where typeName == name
			let callsCreate = CallsCreateModifiedCallback(m.DescendantNodes())
			select new ObjectCreationMethodInfo(){Type=name, MethodName = m.Identifier.ValueText,CallsCreate = callsCreate}
				//.Dump()
		;
		return methodsThatCreateSpecifiedType.ToArray();
}
// Define other methods and classes here
class ModelCollector : CSharpSyntaxWalker
    {
		private ModelCollector():base() {
			
		}
		
		public readonly Dictionary<string,List<string>> implementedInterfaces = new Dictionary<string,List<string>>();

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

		//public override void VisitInterface(
		public override void VisitObjectCreationExpression(ObjectCreationExpressionSyntax node){
			//node.FullSpan.ToString().Dump();
			//base.VisitSimpleBaseType(
			base.VisitObjectCreationExpression(node);
		}

    }