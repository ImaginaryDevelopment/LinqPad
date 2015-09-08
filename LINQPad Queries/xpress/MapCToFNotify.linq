<Query Kind="Program">
  <NuGetReference Prerelease="true">Microsoft.CodeAnalysis</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.CodeAnalysis</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp.Syntax</Namespace>
</Query>

void Main()
{

	var rootPath = @"C:\TFS\Pm-Rewrite\Source-dev-rewrite\PracticeManagement\PracticeManagement.Foundation\DataModels"; // Environment.ExpandEnvironmentVariables("%devroot%");
	rootPath.Dump();
	
	var files = Directory.EnumerateFiles(rootPath,"*DataModel.cs", SearchOption.AllDirectories).Skip(0)//.Take(20)
		;
	files.Count().Dump("checking files");
	var fileToClassesToInterfaces = new Dictionary<string,IDictionary<string,List<string>>>();
	foreach(var file in files){
		// if data model implements ICanAudit, make sure all constructor calls in the DataAccessModel are followed by a call to CreateCallback
		var text = File.ReadAllText(file);
		var tree = CSharpSyntaxTree.ParseText(text);
		//var assemblies = AppDomain.CurrentDomain.GetAssemblies().Select(asm => asm.CodeBase).Dump();
		var root = (CompilationUnitSyntax) tree.GetRoot();
		
		IDictionary<string,List<string>> interfaces = ModelCollector
			.VisitClassInterfaces(root)
			.Where(k => k.Value != null && k.Value.Any(b=> b=="DataModelBase"))
			.ToDictionary(kvp=>kvp.Key, kvp=> kvp.Value);
		if(interfaces.Any()){
			
			fileToClassesToInterfaces[file]= interfaces;
		}
	}
	
	var q= 
		from c in fileToClassesToInterfaces.Keys
		from cls in fileToClassesToInterfaces[c].Keys
		let bases = fileToClassesToInterfaces[c][cls]
		
		where bases.Contains("DataModelBase")
		let text = File.ReadAllText(c)
		let tree = CSharpSyntaxTree.ParseText(text)
		let root = (CompilationUnitSyntax) tree.GetRoot()
		select new{ c, cls,bases, Properties = GetProperties(root), Fields = root.DescendantNodes().OfType<FieldDeclarationSyntax>()};
	q.OrderByDescending(q1 => q1.cls)
//		.Select(q1=> new{ q1.c, q1.cls, CreationMethods= 
//			q1.CreationMethods.Select(cm => 
//				new{ cm.Type,cm.MethodName, CallsCreate = Util.HighlightIf(cm.CallsCreate,cc=> !cc)}
//			)})
//.Dump()
;
	foreach(var cls in q.Take(3)){
		cls.cls.Dump("starting conversion");
		var typeText = new StringBuilder( "type " + cls.cls+"() = \r\n"+"  inherit FSharp.ViewModule.ViewModelBase()\r\n\r\n");
		var fields = cls.Fields.Select(f=> new{FieldDeclaration=f.Declaration.ToFullString(),Declaration=f.Declaration})
			.Select(fd=>new{
				Type=fd.FieldDeclaration.Before(" "),
				Name = fd.FieldDeclaration.After(" ").BeforeOrSelf("="),
				Initial=fd.FieldDeclaration.Contains("=")? fd.FieldDeclaration.After("="):null,
				Declaration = fd.Declaration
				})
				//.Dump("fields")
				;
		// var iNotifyProps = cls.Properties.GroupBy(pInfo =>pInfo.IsINotify);
		foreach(var f in fields)
		{
			var initial = f.Initial;
			if(initial!=null)
			{
				initial=initial.Trim();
			} 
			typeText.AppendLine("  " + ToFField(f.Name, f.Type, initial, f.Declaration));
		}
		//iNotifyProps.Dump();
//		foreach(var p in iNotifyProps.Where(g=> g.Key==false).SelectMany(g=> g)){ // the ones that are NOT INotifyPropertyChanged first
//			if(p.IsSimpleGet)
//				typeText.AppendLine("  let mutable " + p.FieldName + " : "+p.Type+"= null"); //+ "= Unchecked.defaultof<" + p.Type.ToFullString()+">");
//			
//		}
		
		typeText.ToString().Dump(cls.cls);
//		foreach(var p in cls.Properties){
//			
//		}
				
		
	}
}

static string ToFField(string name, string type, string initial, VariableDeclarationSyntax field){
	var fType = ToFType(type);
	var fDec = "let mutable " + name +":";
	if(initial=="string.Empty")
		return fDec + fType+" = String.Empty // string.Empty transform";
	var allNodes = field.DescendantNodes().ToArray();
	var simpleKinds = new[]{ SyntaxKind.NumericLiteralExpression, SyntaxKind.StringLiteralExpression, 
		SyntaxKind.NullLiteralExpression, 
		SyntaxKind.FalseLiteralExpression, SyntaxKind.TrueLiteralExpression};
	var isSimplerInit = allNodes.Length == 2
		&& allNodes[0] is IdentifierNameSyntax
		&& allNodes[1] is VariableDeclaratorSyntax;
	if(isSimplerInit){
		return fDec + fType+ "= null // simpler init";
	}
	
	var isSimpleInit = allNodes.Length == 4 
		&& allNodes[0] is PredefinedTypeSyntax 
		&& allNodes[1] is VariableDeclaratorSyntax 
		&& allNodes[2] is EqualsValueClauseSyntax 
		&& simpleKinds.Contains( allNodes[3].Kind());
	
	if(isSimpleInit)
		return fDec+fType+"="+allNodes[3].ToFullString() +" // simple init";
	var isNullableSimplerInit = allNodes.Length ==3
		&& allNodes[0] is NullableTypeSyntax
		&& allNodes[1] is PredefinedTypeSyntax
		&& allNodes[2] is VariableDeclaratorSyntax;
	if(isNullableSimplerInit){
		return fDec+ "Nullable<"+fType.Before("?")+"> = null // isNullableSimplerInit";
	}
	var isNullableSimpleInit = allNodes.Length == 4
		&& allNodes[0] is NullableTypeSyntax
		&& allNodes[1] is IdentifierNameSyntax
		&& allNodes[2] is VariableDeclaratorSyntax
		&& allNodes[3] is EqualsValueClauseSyntax
		&& allNodes[4].Kind() == SyntaxKind.NullLiteralExpression;
		
	if(isNullableSimpleInit){
		return fDec+"Nullable<"+fType.Before("?")+"> = null // isNullableSimpleInit";
	}
		
	var info = new{ name,type,initial,Declaration = field.ToFullString(), NodeKinds=allNodes.Select(n=> n.Kind())};
	
	if(initial == null)
	{
		info.Dump("no initial");
		return fDec;
	}
	
	var conditional1 = Regex.Match(initial,@"\(?([\w.]+)\s*==\s*(\w+)\)?\s*\?\s*(\w+)\s*:\s*([\w.]+)");
	
	if(conditional1.Success && conditional1.Length==initial.Length){
		var result = fDec +fType+"= if "+conditional1.Groups[1].Value+ "=" + conditional1.Groups[2].Value+ " then "+conditional1.Groups[3].Value + " else "+conditional1.Groups[4].Value;
		new{result,info}.Dump("conditional1");
		return result;
		
	} //else if (conditional1.Success) {
	//	var subCondition = initial.Before(conditional1.Value)+ " if "+conditional1.Groups[1].Value+ "=" + conditional1.Groups[2].Value+ " then "+conditional1.Groups[3].Value + " else "+conditional1.Groups[4].Value + initial.After(conditional1.Value);
	//	new{subCondition,info}.Dump("sub conditional1");
	//}
	else {
		info.Dump("fieldNodes");
	}
//	if(allNodes.Length>3 && allNodes[3].Kind() == SyntaxKind.InvocationExpression)
//	{
//		
//		
//	}
		
	
	
	return fDec +"="+ initial;
}
static string ToFType(string type){
	return type;
}

static string FindFile(string rootPath, string targetFilename){
	var file = Directory.EnumerateFiles(rootPath,targetFilename, SearchOption.AllDirectories).FirstOrDefault();
	return file;
}

static IEnumerable<PropertyInfoB> GetProperties(CompilationUnitSyntax node){
	// assume all props have getters
 	return  from n in node.DescendantNodes().OfType<PropertyDeclarationSyntax>()
			select new PropertyInfoB(n);
		
		//.Dump("simple props")
		;
	
}

class PropertyInfoB {
	readonly AccessorDeclarationSyntax _getter;
	readonly AccessorDeclarationSyntax _setter;
	
	public PropertyInfoB(PropertyDeclarationSyntax prop){
		var accessorCount = prop.AccessorList.Accessors.Count();
		if(accessorCount > 2)
		{
			prop.Dump("too many accessors");
			throw new ArgumentOutOfRangeException();
		}
		
		_getter = prop.AccessorList.Accessors.FirstOrDefault(a=> a.Kind() == SyntaxKind.GetAccessorDeclaration);
		_setter = prop.AccessorList.Accessors.FirstOrDefault(a => a.Kind() == SyntaxKind.SetAccessorDeclaration);
		
		if(_getter!= null)
			IsSimpleGet = IsSimpleGetter(_getter);
			if(IsSimpleGet)
				FieldName = GetSimpleGetFieldName(_getter);
		
		PropertyName = prop.Identifier.ToFullString();
		IsINotify = prop.AccessorList.ToFullString().Contains("SetAndNotify");
		Type = prop.Type.ToFullString();
	}
	
	public bool IsSimpleGet{get;set;}
	public bool IsINotify{get;set;}
	public string Type {get;set;}
	public string FieldName{get;set;}
	public string PropertyName{get;set;}
	
	static bool IsSimpleGetter(AccessorDeclarationSyntax getter){
		var nodes = getter.DescendantNodes().ToArray();
		return !(nodes.Length != 3 || nodes[0] is BlockSyntax == false || nodes[1] is ReturnStatementSyntax == false || nodes[2] is IdentifierNameSyntax ==false);
	}
	
	static string GetSimpleGetFieldName(AccessorDeclarationSyntax getter){
			//TODO: account for autoproperties
			if(!IsSimpleGetter(getter))
				return null;
			//getter.ToFullString().Dump("getter:"+ getter.DescendantNodes().Count()+" dns");
			var nodes =getter.DescendantNodes().ToArray();
			
			var identifierNameNode = (IdentifierNameSyntax)nodes[2];
			//identifierNameNode.Dump();
			var identifier = identifierNameNode.ToFullString();
			if (identifier.StartsWith("_") ==false)
				return null;
			return identifier;
			//dNodes.Dump("dNodes");
			
	}
}
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
    }