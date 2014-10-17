<Query Kind="Statements">
  <NuGetReference>ArchiMetrics.Analysis</NuGetReference>
  <Namespace>Roslyn.Compilers</Namespace>
  <Namespace>Roslyn.Compilers.CSharp</Namespace>
  <Namespace>Roslyn.Services</Namespace>
</Query>

var devRoot = Environment.ExpandEnvironmentVariables("%devroot%");
// HACK: to avoid hard coding paths to test sln
var sln = System.IO.Directory.GetFiles(devRoot,"*.sln",SearchOption.AllDirectories).SkipWhile(d=>d.Contains("Playground")==false).SkipWhile(d=>d.Contains("Playground")).First().Dump("sln file");
ISolution solution;
try
{	        
	solution = Solution.Load(sln);	
}
catch (Exception ex)
{
	if(ex.Message=="Expected Global line.")
	{
		"check for a line like this in your .sln \"VisualStudioVersion = \" or \"MinimumVisualStudioVersion = \"".Dump();
		new LINQPad.Hyperlinq("https://social.msdn.microsoft.com/Forums/en-US/df9af25e-75fc-47e6-9704-8aa89817857c/systemexception-expected-global-line", "help topic").Dump();
	}
	
	throw;
}

//var targetType = typeof(IEnumerable<>);
//targetType.GetMethods().Dump();

//http://ebeid-soliman.blogspot.com/2013/08/getting-started-with-microsoft-roslyn_22.html
(from p in solution.Projects
//var compilation = p.GetCompilation();
from d in p.Documents
let semanticModel = d.GetSemanticModel()
let tree = semanticModel.SyntaxTree
let root = tree.GetRoot()
from c in root.DescendantNodes().OfType<InvocationExpressionSyntax>() // BinaryExpressionSyntax for looking for Count calls
where c.Expression is MemberAccessExpressionSyntax
select new{Project = p.Name, File=d.Name, Expr=c.GetText().ToString()}
).Take(5).Dump();



// of interest:
// http://stackoverflow.com/questions/11174593/whats-the-most-efficient-way-to-build-up-a-idocument-from-the-very-start
// solution.FindSymbols("Count()").Dump();

// var document = solution.GetDocument();
// var root = (CompilationUnitSyntax) document.GetSyntaxRoot();
// root.Dump();

// of interest:
// http://blogs.msdn.com/b/csharpfaq/archive/2011/11/23/using-the-roslyn-symbol-api.aspx
// var compilation = Compilation.Create("S