<Query Kind="Program" />

void Main()
{
var path=Util.ReadLine("SourceDirectory?",@"%devroot%");
var patterns=new[]{"*.cs","*.ascx"};//;*.aspx;*.ascx

Func<string,bool> fileExclude=f=>f.ToLower().EndsWith("designer.cs") 
	|| f.ToLower().StartsWith("jquery-")
	;
	
Func<string,bool> pathExclude=r=>r.Contains("Service References")
	|| r.Contains("Web References")||r.Contains("\\obj\\") ||  r.StartsWith(@"~\Web\PSAT.WebApp\Scripts\Mvc3");
	//System.Environment.GetEnvironmentVariable("devroot", EnvironmentVariableTarget.User).Dump();
foreach(var toReplace in Regex.Matches(path,"%(.*)%").Cast<Match>().Select (m =>new{m.Value,inner= m.Groups[1].Value }))
	path=path.Replace(toReplace.Value,System.Environment.GetEnvironmentVariable(toReplace.inner.Dump("inner"), EnvironmentVariableTarget.Machine)??System.Environment.GetEnvironmentVariable(toReplace.inner, EnvironmentVariableTarget.Process)?? System.Environment.GetEnvironmentVariable(toReplace.inner, EnvironmentVariableTarget.User));

if(System.IO.Directory.Exists(path)==false){
	path.Dump("does not exist");
return;
}

System.Environment.CurrentDirectory=path;
System.Environment.CurrentDirectory.Dump("cd");
	var allResults=RecurseLocation(path,".",patterns);
	var filtered=	allResults.Where (r =>fileExclude(r.FileName)==false 
	&& pathExclude(r.RelativePath)==false);
	filtered.Where(fi=>fi.Lines<600). OrderBy (fi => fi.Lines)
	.Dump("Lowest lines by file")
	;
	var groupedByRelativePath=filtered.GroupBy (r => r.RelativePath)
		.Where (r => r.Sum (x => x.Lines)<400)
		.Select (r =>new {r.Key,Lines=r.Sum (x => x.Lines),Files=r.Select (x => x.FileName)})
		.OrderBy (r => r.Lines)
		.Dump("lowest lines by folder")
		;
	
	var groupedByFilename=filtered.Where (f => f.FileName.Contains(".")).GroupBy (f => f.FileName.Substring(0,f.FileName.IndexOf('.')))
		.Where (f => f.Sum (x => x.Lines)<500)
		.Select (f => new{File=Util.Highlight(f.Key),Lines=f.Sum (x => x.Lines),Nonspaces=f.Sum (x => x.Nonspaces),
			FileDetails=f.Select (x =>new{ x.Lines,x.FileName,x.RelativePath,x.Nonspaces})})
		.OrderBy (f => f.Lines)
		.Dump("lowest lines by filename") ;
		
	//Util.HorizontalRun(true,new{Title="Highest lines by file",groupedByFilename},new{Title=,groupedByRelativePath}, filtered).Dump();
	
}
public struct FileSummary
{
public string RelativePath;
public string FileName;
public int Nonspaces;
public int Lines;
public int DoubleQuotes;
public int PotentialMagicNumbers;
}
// Define other methods and classes here
public static IEnumerable<FileSummary> RecurseLocation(string basePath, string relpath,IEnumerable<string> patterns)
{
	var rgNumber=new Regex(@"\.?[0-9]+(\.[0-9]+)?", RegexOptions.Compiled);
	
foreach(var pattern in patterns)
	foreach(var file in System.IO.Directory.GetFiles(System.IO.Path.Combine(basePath, relpath),pattern))
	{
	var uriPath=(relpath.Length>1?"~"+ relpath.Substring(1):"");
	
	var lines=System.IO.File.ReadAllLines(file);
		//var text=System.IO.File.ReadAllText(file);
		var nonspaces=lines.Sum (text => text.Where(c=>Char.IsWhiteSpace(c)==false).Count());
		var dblQuotes=lines.Sum(text=>text.Where(c=>c=='"').Count());
		
		var magicNumbersRg=lines.Sum(text=>rgNumber.Matches(text).Count);
		yield return new FileSummary(){ RelativePath=uriPath,
										FileName=System.IO.Path.GetFileName(file), 
										Lines=lines.Length, 
										Nonspaces=nonspaces,
										DoubleQuotes=dblQuotes,
										PotentialMagicNumbers=magicNumbersRg,
										};
		
	}
	foreach(var dir in System.IO.Directory.GetDirectories(relpath))
	foreach(var result in  RecurseLocation(basePath,dir,patterns))
	yield return result;
}