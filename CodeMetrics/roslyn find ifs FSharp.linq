<Query Kind="FSharpProgram">
  <NuGetReference>ArchiMetrics.Analysis</NuGetReference>
  <Namespace>Roslyn.Compilers</Namespace>
  <Namespace>Roslyn.Compilers.CSharp</Namespace>
</Query>

// find all ifs that have the return on the same line
// http://blog.filipekberg.se/2011/10/20/using-roslyn-to-parse-c-code-files/

type StringTransform = (string->string)

type SearchOptions =
	|Absolute of string
	|RelativeFile of StringTransform
	|AbsoluteDir of string
	|UseEnvironmentalBase
	|RelativeDir of StringTransform
	|AskForAbsoluteDir of (unit-> string)
	
let analysisSearchOption = SearchOptions.UseEnvironmentalBase

let getFilesToAnalyze so = 

	let baseSearchDir = lazy(System.Environment.ExpandEnvironmentVariables "%devroot%")
	let searchDir d = System.IO.Directory.GetFiles(d,"*.cs", SearchOption.AllDirectories)
	
	match so with
	| UseEnvironmentalBase -> [yield! searchDir baseSearchDir.Value]
	| Absolute(p) -> [ p]
	| RelativeFile(transform) -> [transform baseSearchDir.Value]
	| AbsoluteDir(d) -> [yield! searchDir d]
	| RelativeDir(transform) -> [ yield! transform baseSearchDir.Value |> searchDir]
	| AskForAbsoluteDir t -> [yield! Util.ReadLine() |> searchDir]
	
let getConditionalTokens (p:string) = 
	let myConditional = 
		let syntaxTree = SyntaxTree.ParseFile p
		let root = syntaxTree.GetRoot()
		let descendIntoChildren : System.Func<SyntaxNode,bool> = null
		let nodes = root.DescendantNodes(descendIntoChildren,false)
		nodes.OfType<IfStatementSyntax>()
	myConditional
	
let getSingleLineIfReturns (ifStatementNodes:IfStatementSyntax seq) =
	let ifReturnPattern = @"if\s*\(.*\).*return "
	ifStatementNodes
	|> Seq.filter (fun iss -> iss.ChildNodes().OfType<ReturnStatementSyntax>().Any())
	|> Seq.map(fun iss-> iss.GetText())
	|> Seq.filter(fun text -> text.ToString().IsMatch(ifReturnPattern,false))
	|> Seq.map(fun text -> text.ToString())
	
let replace t (r:string) (str:string) = str.Replace(t,r)

let showNewLines text =
	text 
	|> replace "\r" "\\r"
	|> replace "\n" "\\n"
	
let files =
	getFilesToAnalyze analysisSearchOption
		//.SkipWhile(f=>f.Contains("Validation")==false)
		//.Take(4)
	|> Seq.map(fun f-> f,getConditionalTokens f)
	|> Seq.filter(fun (f,c)-> c.Any())
	|> Seq.map(fun (f,c)-> (f,getSingleLineIfReturns c))
	|> Seq.filter(fun (f,c)-> c.Any())
	|> Seq.map(fun (f,c) -> (f, c |> Seq.map showNewLines))
	
files.Dump()
