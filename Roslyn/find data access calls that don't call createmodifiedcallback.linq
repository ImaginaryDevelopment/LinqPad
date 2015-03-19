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
	var files = 
		//new []{ rootPath + @"\Xpress.Foundation\DataModels\CardiacArrestDataModel.cs"};
		Directory.EnumerateFiles(rootPath,"*DataModel.cs", SearchOption.AllDirectories).Skip(0)//.Take(20)
		;
	//files.Dump();
	var fileToClassesToInterfaces = new Dictionary<string,IDictionary<string,List<string>>>();
	foreach(var file in files){
		// if data model implements ICanAudit, make sure all constructor calls in the DataAccessModel are followed by a call to CreateCallback
		var text = File.ReadAllText(file);
		var tree = CSharpSyntaxTree.ParseText(text);
		//tree.Dump("tree");
		var root = (CompilationUnitSyntax) tree.GetRoot();
		//root.Dump("root");
		//root.DescendantNodes().OfType<PropertyDeclarationSyntax>().Dump();
		IDictionary<string,List<string>> interfaces = ModelCollector
			.VisitClassInterfaces(root)
			.Where(k => k.Value != null && k.Value.Count>1)
			.ToDictionary(kvp=>kvp.Key, kvp=> kvp.Value);
		if(interfaces.Any()){
			
			fileToClassesToInterfaces[file]= interfaces;
		}
		
	}
	//fileToClassesToInterfaces.Dump();
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
	q.Dump();
//	foreach(var c in fileToClassesToInterfaces.Keys)
//		foreach(var cls in fileToClassesToInterfaces[c].Keys)
//		{
//			
//			if(fileToClassesToInterfaces[c][cls].Contains("ICanCreateModificationsCallback") == false)
//				continue;
//			var targetFilename = cls.Before("DataModel")+"DataAccess.cs";
//			
//			var dataAccess = Directory.EnumerateFiles(rootPath,targetFilename, SearchOption.AllDirectories).First();
//			var text = File.ReadAllText(dataAccess);
//			var tree = CSharpSyntaxTree.ParseText(text);
//			var root = (CompilationUnitSyntax) tree.GetRoot();
//			GetObjectCreationsOfType(root,cls).Dump();
//		}
}
static string FindFile(string rootPath, string targetFilename){
	var file = Directory.EnumerateFiles(rootPath,targetFilename, SearchOption.AllDirectories).FirstOrDefault();
	//if (file == null) throw new FileNotFoundException(targetFilename);
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
//		var q= from m in methodNodes
//			from statement in m.Nodes.OfType<InvocationExpressionSyntax>()
//			let stExpr = statement.Expression as MemberAccessExpressionSyntax
//			where stExpr != null
//			let calledMethodNameSyn = stExpr.Name as IdentifierNameSyntax
//			where calledMethodNameSyn != null
//			where calledMethodNameSyn.Identifier.ValueText == "CreateModifiedCallback"
}
	
			
public static IEnumerable<object> GetObjectCreationsOfType(CompilationUnitSyntax node, string name){
	var methodsThatCreateSpecifiedType = from m in node.DescendantNodes().OfType<MethodDeclarationSyntax>()
			
			from ctor in m.DescendantNodes().OfType<ObjectCreationExpressionSyntax>()
			let type = ctor.Type as IdentifierNameSyntax
			where type != null
			let typeName = type.Identifier.ValueText
			where typeName == name
			select new{Type=name, MethodName = m.Identifier.Value,CallsCreate =Util.HighlightIf( CallsCreateModifiedCallback(m.DescendantNodes()),s=>!s)}
				//.Dump()
		;
//	var methodsThatCreateAndCall = 
//			//from maes in m.Select(ies => ies.Expression).OfType<MemberAccessExpressionSyntax>()
//			select new{ m.Type,m.MethodName, calledMethodNameSyn.Identifier.ValueText};
			
//	node.DescendantNodes()
//		.OfType<ObjectCreationExpressionSyntax>()
//		.Select(oces=> oces.Type)
//		.OfType<IdentifierNameSyntax>()
//		.Select(ins => ins.Identifier.ValueText)
//		.Where(x => x==name)
		//new{Identifier = ((SyntaxToken)((IdentifierNameSyntax)oces.Type).Identifier)})
		return methodsThatCreateSpecifiedType.ToArray();
}
// Define other methods and classes here
class ModelCollector : CSharpSyntaxWalker
    {
		private ModelCollector():base() {
			
		}
		
		public readonly Dictionary<string,List<string>> implementedInterfaces = new Dictionary<string,List<string>>();
        //public readonly Dictionary<string, List<string>> models = new Dictionary<string, List<string>>();
		
		public static IDictionary<string,List<string>> VisitClassInterfaces(CompilationUnitSyntax root){
			var mc = new ModelCollector();
			
			mc.Visit(root);
			return mc.implementedInterfaces;
		}
//		public override void VisitPropertyDeclaration(PropertyDeclarationSyntax node){
//			var classnode = node.Parent as ClassDeclarationSyntax;
//            if (!models.ContainsKey(classnode.Identifier.ValueText))
//                models.Add(classnode.Identifier.ValueText, new List<string>());
//				
//			/* character index in file:
//			 	node.FullSpan.Start.Dump(); */
//			
//            models[classnode.Identifier.ValueText].Add(node.Identifier.ValueText);
//			node.Dump("PropertyDeclaration");
//			base.VisitPropertyDeclaration(node);
//		}
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
		public override void VisitInterfaceDeclaration(InterfaceDeclarationSyntax node){
			//node.Dump();
			base.VisitInterfaceDeclaration(node);
		}
		public override void VisitSimpleBaseType(SimpleBaseTypeSyntax node){
			//node.Dump();
			base.VisitSimpleBaseType(node);
		}
		//public override void VisitInterface(
		public override void VisitObjectCreationExpression(ObjectCreationExpressionSyntax node){
			//node.FullSpan.ToString().Dump();
			//base.VisitSimpleBaseType(
			base.VisitObjectCreationExpression(node);
		}
        
		public override void VisitClassDeclaration(ClassDeclarationSyntax node){
			//node.Dump();
			base.VisitClassDeclaration(node);
		}
    }