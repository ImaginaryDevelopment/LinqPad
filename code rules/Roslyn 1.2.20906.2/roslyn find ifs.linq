<Query Kind="Program">
  <NuGetReference>ArchiMetrics.Analysis</NuGetReference>
  <Namespace>Roslyn.Compilers</Namespace>
  <Namespace>Roslyn.Compilers.CSharp</Namespace>
</Query>

// find all ifs that have the return on the same line
void Main()
{
	// http://blog.filipekberg.se/2011/10/20/using-roslyn-to-parse-c-code-files/
	
	var dirToSearch = System.Environment.ExpandEnvironmentVariables("%devroot%")
		;
	dirToSearch.Dump("searching");
	//var singleFile = 	dirToSearch+@"\Managers\Member\MemberManager.cs";
	var files = System.IO.Directory.GetFiles(dirToSearch,"*.cs", SearchOption.AllDirectories);
		//new []{ singleFile};
		//files.Dump();
	files
		//.SkipWhile(f=>f.Contains("Validation")==false)
		//.Take(4)
		.Select(f=>new{File = f,Conditionals= GetConditionalTokens(f)})
		.Where(fc=> fc.Conditionals.Any())
		//.Take(6)
		.Dump();
	
}

public static string ShowNewLines(string input){
	return input.Replace("\r","\\r").Replace("\n","\\n");
}

// Define other methods and classes here
public IEnumerable<string> GetConditionalTokens(string path){
	
	var syntaxTree = SyntaxTree.ParseFile(path);
	var root = syntaxTree.GetRoot();
	var rawText = root.GetText().ToString();
	var myConditional = root.DescendantNodes().OfType<IfStatementSyntax>()
		.Where(iss => iss.ChildNodes().OfType<ReturnStatementSyntax>().Any());
	const string ifReturnPattern = @"if\s*\(.*\).*return ";
	return myConditional.Select(c=>c.GetText())
		.Where(c=>c.ToString().IsMatch(ifReturnPattern,false))
		// some diagnostics
//		.Select(c=>{
//			var pureRaw = c.ToString();
//			var readable = ShowNewLines(pureRaw);
//			new{
//				File=new LINQPad.Hyperlinq(path),
//				c.LineCount,
//				Matches = Regex.Matches( c.ToString(),ifReturnPattern).Cast<Match>().Select(m=>ShowNewLines(m.Value)),
//				readable= Util.OnDemand("readable",()=>readable)}.Dump();
//			return c;
//		})
		.Select(c=>c.ToString());
}