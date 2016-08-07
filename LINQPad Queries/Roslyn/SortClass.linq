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
    var target =
        Util.Cache(() => 
        System.Windows.Forms.Clipboard.GetText()
        )
        ;
    if (!File.Exists(target))
    {
        target.Substring(0, Math.Min(target.Length, 255)).Dump("File not found, expected a path in the clipboard");
        return;
    }
    var rewriter = new SortClassRewriter();
    var tree = CSharpSyntaxTree.ParseText(File.ReadAllText(target));
    var root = tree.GetRoot();
    var visited = rewriter.Visit(root);
    var beautified = new CodeBeautifier().Visit(visited);
    //beautified.ToFullString().Dump();
    visited.ToFullString().Dump();
    
}

//http://www.christophdebaene.com/blog/2011/10/26/roslyn-formatting-code/
// close but not quite, needs work
public class CodeBeautifier : CSharpSyntaxRewriter
{
    public override SyntaxToken VisitToken(SyntaxToken token)
    {
        switch (token.Kind())
        {
            case SyntaxKind.SemicolonToken:

                if (token.GetPreviousToken().Kind() == SyntaxKind.CloseBraceToken ||
                   token.GetPreviousToken().Kind() == SyntaxKind.CloseParenToken)
                {
                    return token
                    .WithLeadingTrivia()
                    .WithTrailingTrivia(Microsoft.CodeAnalysis.CSharp.SyntaxFactory.ElasticCarriageReturnLineFeed);
                }

                break;

            case SyntaxKind.CloseBraceToken:

                if (token.GetNextToken().Kind() == SyntaxKind.CloseParenToken ||
                   token.GetNextToken().Kind() == SyntaxKind.SemicolonToken)
                {
                    return token
                    .WithTrailingTrivia();
                }

                break;

            case SyntaxKind.CloseParenToken:

                if (token.GetPreviousToken().Kind() == SyntaxKind.CloseBraceToken)
                {
                    return token
                    .WithLeadingTrivia();
                }

                break;
        }

        return token;
    }
}
// Define other methods and classes here
public class SortClassRewriter : CSharpSyntaxRewriter
{
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
            var fds = (FieldDeclarationSyntax)mds;
            if (fds.Declaration.Variables.Count == 1)
                return fds.Declaration.Variables[0].Identifier.ValueText;
            return String.Empty;
        }

        if (mds is MethodDeclarationSyntax)
        {
            var meth = (MethodDeclarationSyntax)mds;
            return meth.Identifier.ValueText;
        }
        if (mds is PropertyDeclarationSyntax)
        {
            var pds = (PropertyDeclarationSyntax) mds;
            return pds.Identifier.ValueText;
        }

        return null;
    }

    public static string GetReturnType(MemberDeclarationSyntax mds)
    {
        if (mds is FieldDeclarationSyntax)
        {
            var fds = (FieldDeclarationSyntax) mds;
            var vds = (VariableDeclarationSyntax)fds.Declaration;
            //vds.Type.Dump();
            return vds.Type.GetText().ToString().Trim();
        }
        if (mds is MethodDeclarationSyntax)
        {
            var meth = (MethodDeclarationSyntax) mds;
            return meth.ReturnType.GetText().ToString().Trim();
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
        Func<MemberDeclarationSyntax, string, bool> isEventMethod = (sl, name) =>
        {
            if (name == null)
            return false;
            if(!(sl is MethodDeclarationSyntax))
            return false;
            var mds = (MethodDeclarationSyntax) sl;
            if(!mds.ParameterList.Parameters.Any())
                return false;
            //mds.ParameterList.Parameters[0].Type.Dump();
            var pdts = mds.ParameterList.Parameters[0].Type as PredefinedTypeSyntax;
            
            return mds.ParameterList.Parameters[0].Identifier.ValueText=="sender" && pdts !=null && pdts.Keyword.ValueText=="object";
        };
        
        var ordered = from sl in syntaxList2
                      let returnType = GetReturnType(sl)
                      let syntaxTypeName = sl.GetType().Name
                      let name = GetName(sl)
                      let fieldTypeName = syntaxTypeName == nameof(FieldDeclarationSyntax) ? GetReturnType(sl) : string.Empty
                      let toDump = new { name, syntaxTypeName, fieldTypeName}
                      orderby syntaxTypeName == nameof(FieldDeclarationSyntax) descending,
                            syntaxTypeName == nameof(ConstructorDeclarationSyntax) descending,
                            syntaxTypeName == nameof(PropertyDeclarationSyntax) descending,
                            string.IsNullOrEmpty(fieldTypeName)?false : char.IsUpper(fieldTypeName[0]),
                            name == nameof(IDisposable.Dispose) || name=="IsDisposed",
                            isEventMethod(sl,name),
                            name != null && name.Contains("Execute"),
                            name
                      select new { sl, toDump};
        
        var syntaxListOrdered = new SyntaxList<MemberDeclarationSyntax>();
        
        foreach (var orderedSyntax in ordered.ToArray())
        {
            // trying to fix newline formatting seems to be a headache at this time
            syntaxListOrdered = syntaxListOrdered.Add(orderedSyntax.sl);
        }
        SyntaxToken syntaxToken3 = this.VisitToken(node.CloseBraceToken);
        SyntaxToken syntaxToken4 = this.VisitToken(node.SemicolonToken);
        return node.Update(syntaxList, syntaxTokenList, syntaxToken, syntaxToken1, typeParameterListSyntax, baseListSyntax, syntaxList1, syntaxToken2,
            syntaxListOrdered,
            syntaxToken3, syntaxToken4);
    }

    //public override SyntaxNode
}