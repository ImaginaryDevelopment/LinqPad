<Query Kind="Statements">
<Query Kind="Program">
  <Connection>
    <IncludeSystemObjects>true</IncludeSystemObjects>
  </Connection>
  <NuGetReference>ArchiMetrics.Analysis</NuGetReference>
  <Namespace>Roslyn.Compilers</Namespace>
  <Namespace>Roslyn.Compilers.CSharp</Namespace>
</Query>

void Main()
{
	// http://blog.filipekberg.se/2011/10/20/using-roslyn-to-parse-c-code-files/
	var linq2Sql=this;
	linq2Sql.DeferredLoadingEnabled=false;
	linq2Sql.ObjectTrackingEnabled=false;
	//mate up calls to stored procedure with parameters passed
	
	var dirToSearch = 
	var files = System.IO.Directory.GetFiles(dirToSearch,"*.cs", SearchOption.AllDirectories);
		//new []{ singleFile};
	
	var fileAnalysisResults = files.Select(f=>
	{
	
		var literalInfos = GetLiteralTokens(f);
		return new FileAnalysisResult(){ File=f, MethodAnalyses=AnalyzeMethodsAgainstSql(literalInfos.Literals,linq2Sql)};
	}).ToArray();
	var withAnalyses = fileAnalysisResults.Where(far => far.MethodAnalyses.Any()).Dump("with analyses");
	//fileAnalysisResults.OrderByDescending(far=>far.MethodAnalyses.Any()).Dump();
}

public class FileAnalysisResult{
	public string File{get;set;}
	public IEnumerable<MethodAnalysisResult> MethodAnalyses{get;set;}
}


public class MethodAnalysisResult{
	public string MethodName{get;set;}
	public int MethodLine{get;set;}
	public string SchemaName{get;set;}
	public string SprocName{get;set;}
	public bool ProcNotFound {get;set;}
	public string InCodeNotInProcedure{get;set;}
	public string InProcedureNotInCode{get;set;}
	public string MethodLiterals{get;set;}
	public string ProcParameters{get;set;}
	public string ProcDeclarationCleaned{get;set;}
	public string ProcDeclaration{get;set;}
	
	public DumpContainer ProcBody{get;set;}
	public DumpContainer MethodBody{get;set;}
	
}

public static IEnumerable<MethodAnalysisResult> AnalyzeMethodsAgainstSql(IEnumerable<LiteralInfo> literalInfos, UserQuery dc){
	var procDeclarationRegex =new  Regex("$\\s*as\\s*^", RegexOptions.Multiline| RegexOptions.IgnoreCase);
	var procParameterRegex = new Regex("@\\w+");
	foreach(var literalInfo in literalInfos?? Enumerable.Empty<LiteralInfo>()){
	
		var procName = literalInfo.ProcNameWithSchema.After(".").AfterOrSelf("[").BeforeOrSelf("]");
		var schemaName = literalInfo.ProcNameWithSchema.Before(".").AfterOrSelf("[").BeforeOrSelf("]");
		var q = (from scomm in dc.sys.Syscomments
			join sp in dc.sys.Procedures on scomm.Id equals sp.Object_id
			join schema in dc.sys.Schemas on sp.Schema_id equals schema.Schema_id
			where sp.Name==procName && schema.Name==schemaName && scomm.Number<=1 && scomm.Colid<=1
			select new {scomm.Text,SprocName=sp.Name,SchemaName=schema.Name}).ToArray();
		if(q.Count()>1)
			throw new InvalidOperationException(literalInfo.ProcNameWithSchema + " found multiple times");
		if(q.Any()==false)
		{
			yield return new MethodAnalysisResult(){ MethodName=literalInfo.MethodName,
				ProcNotFound=true};
			continue;
		}
		var procInfo = q.First();
		
		var inCodeNotInProc = literalInfo.Arguments.Where(arg=> procInfo.Text.Contains(arg)==false);
		
		var regexMatch = procDeclarationRegex.Match(procInfo.Text);
		
		var procDeclaration =procInfo.Text.Substring(0,regexMatch.Index);  // procInfo.Text.Before("as", StringComparison.InvariantCultureIgnoreCase);
		var cleanedComments = procDeclaration.SplitLines().Select(l=>l.BeforeOrSelf("--")).Aggregate((s1,s2)=> s1+Environment.NewLine+s2); //strip comments
		//only accounts for one block comment not nested
		var cleanedBlockComments = cleanedComments.Contains("/*")? cleanedComments.Before("/*")+cleanedComments.After("*/"):cleanedComments;
		var sqlParameters =procParameterRegex.Matches(cleanedBlockComments).Cast<Match>().Select(m=>m.Value).DumpIf(s=>s.Contains(","));
		var inProcNotInCode =sqlParameters.Where(procArg => literalInfo.Arguments.Any(arg=>procArg.Trim()==arg.Trim())==false);
				
		if( inCodeNotInProc.Any() || inProcNotInCode.Any())
			yield return new MethodAnalysisResult(){
				
				MethodName=literalInfo.MethodName,
				ProcParameters=sqlParameters.Any()? sqlParameters.Aggregate((s1,s2)=>s1+","+s2):string.Empty,
				MethodLine=literalInfo.LineNumber,
				MethodBody=Util.OnDemand("methodBody",()=>literalInfo.MethodBody),
				SchemaName=procInfo.SchemaName,
				SprocName= procInfo.SprocName,
				InCodeNotInProcedure=inCodeNotInProc.Any()? inCodeNotInProc.Aggregate((s1,s2) => s1+","+s2):string.Empty,
				InProcedureNotInCode=inProcNotInCode.Any()? inProcNotInCode.Aggregate((s1,s2) => s1+","+s2):string.Empty,
				ProcDeclarationCleaned=cleanedBlockComments,
				ProcDeclaration=procDeclaration,
				ProcBody=Util.OnDemand("procBody",()=> procInfo.Text),
				MethodLiterals = literalInfo!=null && literalInfo.Arguments.Any()? literalInfo.Arguments.Aggregate((s1,s2)=> s1+","+s2):string.Empty
				};
	}
}
//public static IEnumerable<string> GetSqlParameters(string procDeclaration){
//	
//}
public class LiteralInfo{
	public string MethodName{get;set;}
	public int LineNumber{get;set;}
	public string ProcNameWithSchema{get;set;}
	public string MethodBody{get;set;}
	public IEnumerable<string> Arguments {get;set;}
}

public class GetLiteralTokensResult{
	public bool NoClassFound {get;set;}
	public bool NoMethodsFound {get;set;}
	public IEnumerable<LiteralInfo> Literals{get;set;}
}

// Define other methods and classes here
public GetLiteralTokensResult GetLiteralTokens(string path){
	
	var syntaxTree = SyntaxTree.ParseFile(path);
	var root = syntaxTree.GetRoot();
	var myClass = root.DescendantNodes().OfType<ClassDeclarationSyntax>().FirstOrDefault();
	if(myClass==null) { 
		return new GetLiteralTokensResult(){ NoClassFound=true};
	}
	var myMethods = myClass.DescendantNodes().OfType<MethodDeclarationSyntax>();
	if(myMethods.Any()==false){
		return new GetLiteralTokensResult(){ NoMethodsFound=true};
	}
	var sprocCallingMethods = myMethods.Where(mm =>mm.Body!=null && mm.Body.GetText().ToString().Contains(".CommandText"));
	var literalInfos=sprocCallingMethods.Select( scm =>new{ 
				scm.Identifier.ValueText,
				scm.GetLocation().GetLineSpan(true).StartLinePosition.Line,
				Body = scm.GetText().ToString(),
				Literals= scm.DescendantNodes()
			.OfType<LiteralExpressionSyntax>() 
			.Select(les => les.Token)
			.Where(token => token.Kind== SyntaxKind.StringLiteralToken)
			.Select(token=>token.ValueText)
			.Where(s=> s.Contains(".") || s.StartsWith("@"))}
			)
		.Where(pair=>pair.Literals.Any(literal =>literal.Contains(".")))
		.Select(x=>
			new LiteralInfo{
				MethodName=x.ValueText, 
				LineNumber=x.Line,
				MethodBody=x.Body,
				ProcNameWithSchema=x.Literals.First(l=>l.Contains(".")),
				Arguments=x.Literals.Where(l=>l.Contains(".")==false)
			}
		);
	return new GetLiteralTokensResult(){Literals= literalInfos};
}