<Query Kind="Statements">
</Query>

//mate up calls to stored procedure with parameters passed

var text = System.IO.File.ReadAllLines(path);

var syntaxTree =SyntaxTree.ParseFile(path);
var root = syntaxTree.GetRoot();
var myClass = root.DescendantNodes().OfType<ClassDeclarationSyntax>().First();
var myMethods = myClass.DescendantNodes().OfType<MethodDeclarationSyntax>();
var sprocCallingMethods = myMethods.Where(mm => mm.Body.GetText().ToString().Contains(".CommandText"));
sprocCallingMethods.Select( scm =>new{ scm.Identifier.ValueText,Literals= scm.DescendantNodes()
	.OfType<LiteralExpressionSyntax>() 
	.Select(les => les.Token)
	.Where(token => token.Kind== SyntaxKind.StringLiteralToken)
	.Select(token=>token.ValueText)
	.Where(s=> s.Contains(".") || s.StartsWith("@"))}
).Dump();