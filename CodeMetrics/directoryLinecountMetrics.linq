<Query Kind="Program" />

const int HIGHEST_LINES_BY_FILE_MINIMUM=550;
const int HIGHEST_LINES_BY_FOLDER_MINIMUM=2000;
const int HIGHEST_MAGIC_BY_FILE_MINIMUM=6;
public class CountSettings
{
public CountSettings(){
pathExpanded=path.Contains("%")? System.Environment.ExpandEnvironmentVariables(path):path;
foreach(var toReplace in Regex.Matches(pathExpanded,"%(.*)%").Cast<Match>().Select (m =>new{m.Value,inner= m.Groups[1].Value }))
	pathExpanded=path.Replace(toReplace.Value,System.Environment.GetEnvironmentVariable(toReplace.inner.Dump("inner"), EnvironmentVariableTarget.Machine)??System.Environment.GetEnvironmentVariable(toReplace.inner, EnvironmentVariableTarget.Process)?? System.Environment.GetEnvironmentVariable(toReplace.inner, EnvironmentVariableTarget.User));

}
	readonly string path=Util.ReadLine("SourceDirectory?",@"%devroot%");
	readonly string pathExpanded;
	
	readonly IEnumerable<string> patterns=new[]{"*.cs","*.aspx","*.ascx","*.js"};//;*.aspx;*.ascx

	readonly Func<string,bool> fileExclude=f=>
		f.EndsWith("designer.cs",StringComparison.CurrentCultureIgnoreCase) 
	 || f.StartsWith("jquery-",StringComparison.CurrentCultureIgnoreCase)
	 || f.StartsWith("AssemblyInfo",StringComparison.CurrentCultureIgnoreCase) 
	 || f.EndsWith("generated.cs",StringComparison.CurrentCultureIgnoreCase) 
	 || f.EndsWith("codegen.cs",StringComparison.CurrentCultureIgnoreCase) 
	 || f.Contains("jquery")
	 || f.EndsWith(".js")
	 || f=="T4MVC.cs";
	 
	 readonly Func<string,bool> pathExclude=r=>
	   	   r.Contains("Service References")
		|| r.Contains("$tf")
		|| r.Contains(".git")
		|| r.Contains("Web References") 
		|| r.Contains("PackageTmp") 
		|| r.Contains(@"\Database\")
		|| r.Contains(@"\Scripts\Mvc3")
		|| r.Contains("jquery") 
		|| r.Contains("Generated_C#_Source");
	
	 	readonly IEnumerable<string> specialHandling=new[]{"*.mrx"};
		
		public string Path{get{return pathExpanded;}}
		public IEnumerable<string> Patterns{get{return patterns;}}
		public Func<string,bool> FileExclude{get{return fileExclude;}}
		public Func<string,bool> PathExclude{get{return pathExclude;}}
}

void Main()
{

 var settings= new CountSettings();
 settings.Path.Dump("Searching");
	
if(System.IO.Directory.Exists(settings.Path)==false)
return;

System.Environment.CurrentDirectory=settings.Path;
directoriesSearched.Clear();
	var allResults=RecurseLocation(settings.Path,".",settings.Patterns);
		allResults.Count().Dump("Total files found");
		
		directoriesSearched.Dump("DirectoriesSearched "+ directoriesSearched.Count);
	var filtered=	allResults.Where (r =>settings.FileExclude(r.FileName)==false 
		&& settings.PathExclude(r.RelativePath)==false);
	//filtered.GroupBy(f=>f.RelativePath).Dump();
	//filtered.Take(800).Dump();
	//filtered.Skip(800).Dump();
	//return;
	filtered.Count().Dump("Total files included");
	
	GetHighestLinesByFile(filtered).Dump("Highest lines by file > "+HIGHEST_LINES_BY_FILE_MINIMUM);
	
	GetByRelativePath(filtered).Dump("highest lines by folder > "+ HIGHEST_LINES_BY_FOLDER_MINIMUM);
	
	GetHighestLinesByFileBase(filtered).Dump("highest lines by filebase > "+HIGHEST_LINES_BY_FILE_MINIMUM) ;
	filtered.First().Dump();
	GetHighestMagicByFile(filtered).Dump("Highest magic by file >"+HIGHEST_MAGIC_BY_FILE_MINIMUM);
	//Util.HorizontalRun(true,new{Title="Highest lines by file",groupedByFilename},new{Title=,groupedByRelativePath}, filtered).Dump();
	
}

public class FilenameGrouping
{
	public object File{get; private set;}
	//public string File{get;private set;}
	public int Lines{get; private set;}
	public int Nonspaces{get;private set;}
	public IEnumerable<FilenameDetail> FilenameDetails {get; private set;}
	public class FilenameDetail
	{
		public int Lines{get;private set;}
		public string FileName{get;private set;}
		public string RelativePath{get;private set;}
		public int Nonspaces{get;private set;}
		public FilenameDetail(int lines, string filename, string relativePath,int nonspaces)
		{
			Lines=lines;
			FileName=filename;
			RelativePath=relativePath;
			Nonspaces=nonspaces;
		}
	}
	public FilenameGrouping(string file, int lines, int nonspaces, IEnumerable<FilenameDetail> filenameDetails)
	{
		File=Util.Highlight(file);
		Lines=lines;
		Nonspaces=nonspaces;
		FilenameDetails=filenameDetails;
	}
}

public IEnumerable<FilenameGrouping> GetHighestLinesByFileBase(IEnumerable<FileSummary> files)
{
	return files.Where (f => f.FileName.Contains(".")).GroupBy (f => f.FileName.Substring(0,f.FileName.IndexOf('.')))
		.Where (f => f.Sum (x => x.Lines)>HIGHEST_LINES_BY_FILE_MINIMUM)
		.Select (f => new FilenameGrouping(f.Key,f.Sum (x => x.Lines)
			,f.Sum (x => x.Nonspaces)
			,f.Select (x =>new FilenameGrouping.FilenameDetail(x.Lines,x.FileName,x.RelativePath,x.Nonspaces))
				.OrderBy(fd=>fd.RelativePath)
			))
		.OrderByDescending (f => f.Lines);
}

public IEnumerable<FileSummary> GetHighestMagicByFile(IEnumerable<FileSummary> files)
{
	return files.Where(fi=>fi.PotentialMagicNumbers+(fi.DoubleQuotes/2)>HIGHEST_MAGIC_BY_FILE_MINIMUM). OrderByDescending (fi => fi.PotentialMagicNumbers+(fi.DoubleQuotes/2));
}
public IEnumerable<FileSummary> GetHighestLinesByFile(IEnumerable<FileSummary> files)
{
	return files.Where(fi=>fi.Lines>HIGHEST_LINES_BY_FILE_MINIMUM). OrderByDescending (fi => fi.Lines);
}


public IOrderedEnumerable<StringGrouping> GetByRelativePath(IEnumerable<FileSummary> files)
{
return files.GroupBy (r => r.RelativePath)
		.Where (r => r.Sum (x => x.Lines)>HIGHEST_LINES_BY_FOLDER_MINIMUM)
		.Select (r =>new StringGrouping(r.Key,r.Sum (x => x.Lines),r.Select (x => x.FileName)))
		.OrderByDescending (r => r.Lines);
}

static readonly IList<string> directoriesSearched=new List<string>();
// Define other methods and classes here
public static IEnumerable<FileSummary> RecurseLocation(string basePath, string relpath,IEnumerable<string> patterns)
{
	var rgNumber=new Regex(@"\.?[0-9]+(\.[0-9]+)?", RegexOptions.Compiled);
	var uriPath=(relpath.Length>1?"~"+ relpath.Substring(1):"");
	
foreach(var pattern in patterns)
	foreach(var file in System.IO.Directory.GetFiles(System.IO.Path.Combine(basePath, relpath),pattern))
	{
	
	
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
	{
		directoriesSearched.Add(dir);
		
	foreach(var result in  RecurseLocation(basePath,dir,patterns))
	yield return result;
	}
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

public class StringGrouping
{
	public string Key{get; private set;}
	public int Lines{get;private set;}
	public IEnumerable<string> Files{get;private set;}
	
	public StringGrouping(string key, int lines,IEnumerable< string> files)
	{
		Key=key;
		Lines=lines;
		Files=files;
	}
}