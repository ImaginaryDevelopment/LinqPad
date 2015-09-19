<Query Kind="Program">
  <NuGetReference>Microsoft.CodeAnalysis.CSharp</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.CodeAnalysis</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp.Syntax</Namespace>
</Query>

void Main()
{
	var rootPath = @"C:\tfs\Pm-Rewrite\Source-dev-rewrite\PracticeManagement\";
	var propsSeq = Directory.GetFiles(rootPath, "*.cs", SearchOption.AllDirectories).Where(PathFilter).Select(f =>
	{
		var code = File.ReadAllText(f);
		return new { f, Properties = ModelCollector.VisitProperties(code), Methods = ModelCollector.VisitMethods(code), Invocations = ModelCollector.VisitInvocations(code) };
	})
	//.Where(x => x.Properties.Any())
	.Take(6)
	.Dump();
}

bool PathFilter(string path)
{
	return path.Contains("CodedUITest") == false && path.Contains("ViewModelsC") == false;
}

// Define other methods and classes here
class ModelCollector : CSharpSyntaxWalker
{
	readonly Dictionary<string, List<string>> classProperties = new Dictionary<string, List<string>>();
	readonly Dictionary<string, List<string>> classMethods = new Dictionary<string, List<string>>();
	readonly Dictionary<string, List<string>> classMethodInvocations = new Dictionary<string, List<string>>();

	public static IDictionary<string, List<string>> VisitProperties(string text)
	{
		var tree = CSharpSyntaxTree.ParseText(text);
		var root = (CompilationUnitSyntax)tree.GetRoot();
		
		var mc = new ModelCollector();
		mc.Visit(root);
		return mc.classProperties;
	}

	public static IDictionary<string, List<string>> VisitMethods(string text)
	{
		var tree = CSharpSyntaxTree.ParseText(text);
		var root = (CompilationUnitSyntax)tree.GetRoot();
		var mc = new ModelCollector();
		mc.Visit(root);
		return mc.classMethods;
	}

	public static IDictionary<string, List<string>> VisitInvocations(string text)
	{
		var tree = CSharpSyntaxTree.ParseText(text);
		var root = (CompilationUnitSyntax)tree.GetRoot();
		var mc = new ModelCollector();
		mc.Visit(root);
		return mc.classMethodInvocations;
	}

	static void AddToLookup<T>(Dictionary<string, List<T>> dic, string key, T value)
	{
		if (!dic.ContainsKey(key))
			dic.Add(key, new List<T>());
		dic[key].Add(value);
	}

	static ClassDeclarationSyntax FindClassParent(SyntaxNode node)
	{
		if (node.Parent is ClassDeclarationSyntax)
			return (ClassDeclarationSyntax)node.Parent;
		return FindClassParent(node.Parent);
	}

	public override void VisitInvocationExpression(InvocationExpressionSyntax node)
	{
		var classnode = FindClassParent(node);
		if (classnode == null)
			node.Dump();
		var className = classnode.Identifier.ValueText;
		var ins = node.Expression as IdentifierNameSyntax;
		if (ins != null)
		{
			AddToLookup(classMethodInvocations, className,"this."+ ins.Identifier.ValueText);
		}
		else if (node.Expression is MemberAccessExpressionSyntax)
		{
			var maes = (MemberAccessExpressionSyntax)node.Expression;
			var parent = node.Parent;
			if (maes.Expression is IdentifierNameSyntax)
			{
				var identifier = ((IdentifierNameSyntax)maes.Expression).Identifier.ValueText;
				AddToLookup(classMethodInvocations, className, "id:" + identifier + maes.OperatorToken + maes.Name.Identifier.ValueText);
			}
			else if (maes.Expression is BaseExpressionSyntax)
			{
				AddToLookup(classMethodInvocations, className, "base:" + maes.ToString());
			}
			else if (maes.Expression is MemberAccessExpressionSyntax)
			{
				AddToLookup(classMethodInvocations, className, maes.ToString());
			}
			else if (maes.Expression is ThisExpressionSyntax)
			{
				var tes = ((ThisExpressionSyntax) maes.Expression).Token.ValueText;
				AddToLookup(classMethodInvocations, className, tes+maes.OperatorToken+maes.Name.Identifier.ValueText);
			}
			else if (maes.Expression is ParenthesizedExpressionSyntax)
			{
				AddToLookup(classMethodInvocations, className, maes.ToString());
			}
			else if (maes.Expression is PredefinedTypeSyntax)
			{
				if(maes.Expression.ToString() != "string")
					maes.Expression.Dump("unknown predefinedtypesyntax");
			}
			else
			{
				maes.Expression.Dump("unknown maes");
				AddToLookup(classMethodInvocations, className,"Maes:"+ maes.ToString());
			}
		}
		else
		{
			node.Expression.Dump("unknown invocation type");
		}

		base.VisitInvocationExpression(node);
	}

	public override void VisitPropertyDeclaration(PropertyDeclarationSyntax node)
	{

		var classnode = (ClassDeclarationSyntax)node.Parent;
		var className = classnode.Identifier.ValueText;
		AddToLookup(classProperties, className, node.Identifier.ValueText);

		/* character index in file:
			node.FullSpan.Start.Dump(); */
	}

	/* since we get no intellisense for override declarations */
	public override void VisitMethodDeclaration(MethodDeclarationSyntax node)
	{
		var classnode = node.Parent as ClassDeclarationSyntax;

		if (classnode == null)
		{
			node.Dump("parent is not a class");

		}
		else
		{
			var className = classnode.Identifier.ValueText;
			AddToLookup(classMethods, className, node.Identifier.ValueText);
		}

		base.VisitMethodDeclaration(node);
	}

	public override void VisitClassDeclaration(ClassDeclarationSyntax node)
	{
		base.VisitClassDeclaration(node);
	}
}