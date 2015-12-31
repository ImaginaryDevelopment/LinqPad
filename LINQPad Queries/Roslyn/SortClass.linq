<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>Microsoft.CodeAnalysis.CSharp</NuGetReference>
  <Namespace>Microsoft.CodeAnalysis</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp</Namespace>
  <Namespace>Microsoft.CodeAnalysis.CSharp.Syntax</Namespace>
</Query>

void Main()
{
	// only have to have the file path on the clipboard the first time, then F5 will just keep using the same one
	var target = Util.Cache(() => System.Windows.Forms.Clipboard.GetText());
	var rewriter = new SortClassRewriter();
	var tree = CSharpSyntaxTree.ParseText(File.ReadAllText(target));
	var visited = rewriter.Visit(tree.GetRoot());
	visited.ToFullString().Dump();
}

// Define other methods and classes here
public class SortClassRewriter : CSharpSyntaxRewriter
{
	// this approach won't work with so don't handle nested classes for now
	ClassDeclarationSyntax _class = null;

	public override SyntaxNode Visit(SyntaxNode node)
	{
		if (node is RegionDirectiveTriviaSyntax)
		{
			return node.RemoveNode(node.Parent, SyntaxRemoveOptions.KeepNoTrivia);
		}
		return base.Visit(node);
	}

	public override SyntaxTrivia VisitTrivia(SyntaxTrivia trivia)
	{
		var isRegionOrEndRegion = trivia.HasStructure && trivia.Kind() == SyntaxKind.RegionDirectiveTrivia || trivia.Kind() == SyntaxKind.EndRegionDirectiveTrivia;
		if (isRegionOrEndRegion)
		{
			return default(SyntaxTrivia);
		}
		else
		{
			return base.VisitTrivia(trivia);
		}
	}

	public static string GetName(MemberDeclarationSyntax mds)
	{
		if (mds is FieldDeclarationSyntax)
		{
			var fds = (FieldDeclarationSyntax) mds;
			if(fds.Declaration.Variables.Count == 1)
				return fds.Declaration.Variables[0].Identifier.ValueText;
			return String.Empty;
		}
		
		if (mds is MethodDeclarationSyntax)
		{
			var meth = (MethodDeclarationSyntax)mds;
			return meth.Identifier.ValueText;
		}
		
		return null;
	}
	
	public override SyntaxNode VisitClassDeclaration(ClassDeclarationSyntax node)
	{
		SyntaxList<AttributeListSyntax> syntaxList = this.VisitList<AttributeListSyntax>(node.AttributeLists);
		SyntaxTokenList syntaxTokenList = this.VisitList(node.Modifiers);
		SyntaxToken syntaxToken = this.VisitToken(node.Keyword);
		SyntaxToken syntaxToken1 = this.VisitToken(node.Identifier);
		TypeParameterListSyntax typeParameterListSyntax = (TypeParameterListSyntax)this.Visit(node.TypeParameterList);
		BaseListSyntax baseListSyntax = (BaseListSyntax)this.Visit(node.BaseList);
		SyntaxList<TypeParameterConstraintClauseSyntax> syntaxList1 = this.VisitList<TypeParameterConstraintClauseSyntax>(node.ConstraintClauses);
		SyntaxToken syntaxToken2 = this.VisitToken(node.OpenBraceToken);
		SyntaxList<MemberDeclarationSyntax> syntaxList2 = this.VisitList<MemberDeclarationSyntax>(node.Members);
		var ordered = from sl in syntaxList2
			let type = sl.GetType().Name
			let name = GetName(sl)
			orderby type=="FieldDeclarationSyntax" descending,name
			select sl;
		
		//ordered.GroupBy(o => o.GetType().Name, o => o.ToFullString()).OrderBy(g =>g.Key =="FieldDeclarationSyntax").Dump("ordered");

		var syntaxListOrdered = new SyntaxList<MemberDeclarationSyntax>();

		foreach (var sl in ordered.ToArray())
			syntaxListOrdered = syntaxListOrdered.Add(sl);

		SyntaxToken syntaxToken3 = this.VisitToken(node.CloseBraceToken);
		SyntaxToken syntaxToken4 = this.VisitToken(node.SemicolonToken);
		return node.Update(syntaxList, syntaxTokenList, syntaxToken, syntaxToken1, typeParameterListSyntax, baseListSyntax, syntaxList1, syntaxToken2,
			syntaxListOrdered,
			syntaxToken3, syntaxToken4);
	}

	//public override SyntaxNode
}