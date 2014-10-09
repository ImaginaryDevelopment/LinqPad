<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
</Query>

static bool debug = true;
static bool runRedirected = true;

void Main()
{

DateTime? backdateForAnalysis = new DateTime(2013,11,19);
string slnPath;
var sonarRunnerHome = System.Environment.ExpandEnvironmentVariables("%SONAR_RUNNER_HOME%").Dump("sonar runner home");
//// sonar wants to create a temp directory to write things into
//Environment.CurrentDirectory = sonarRunnerHome;
using(var ofd=new System.Windows.Forms.OpenFileDialog())
{
	var devroot = System.Environment.ExpandEnvironmentVariables("%devroot%");
	if(System.IO.Directory.Exists(devroot))
	ofd.InitialDirectory=devroot;
	if(ofd.ShowDialog()!= System.Windows.Forms.DialogResult.OK)
	{
		"Process cancelled".Dump();
		return;
	}
	slnPath=ofd.FileName;
}

var slnDirectory=System.IO.Path.GetDirectoryName(slnPath);

Environment.CurrentDirectory = slnDirectory;

slnDirectory.Dump();

var propertiesPath=System.IO.Path.Combine(slnDirectory,"sonar-project.properties");

//if(System.IO.File.Exists(propertiesPath))
//	EditSonarProperties(propertiesPath);
//else
	CreateSonarProperties(propertiesPath, System.IO.Path.GetFileName(slnPath),backdateForAnalysis);

	RunSonarRunner(slnDirectory);
	
	
} //main

//public static void EditSonarProperties(string propertiesPath)
//{
//
//}

public static void RunSonarRunner(string slnDirectory)
{
var psi=new ProcessStartInfo(@"C:\projects\sonar\sonar-runner-2.4\bin\sonar-runner.bat"){ RedirectStandardError=runRedirected, RedirectStandardOutput=runRedirected//,RedirectStandardInput=true 
		, UseShellExecute=!runRedirected,ErrorDialog=false, CreateNoWindow= !debug} ; // WinMgmt or WMSvc?

	using (var ps=new Process())
	{
	ps.StartInfo=psi;
	if(runRedirected){
	//var input=ps.StandardInput;
	 var queryOutputs=ps.RunProcessRedirected(debug? "-X": string.Empty); //string.Empty);
	if(queryOutputs.Errors.HasValue()) return;
	} else {
	
	ps.Start();
	
	//ps.StandardError.ReadToEnd().Dump("stderr");
	//ps.StandardOutput.ReadToEnd().Dump("stdout");
	
	}
	
	//	startOutput=ps.RunProcessRedirected(@"\\"+server+" "+current+" "+toStart);
		
	}	//ps disposed
	
	
}

public static void CreateSonarProperties(string propertiesPath, string projectName, DateTime? projectDate = null)
{ // https://github.com/SonarSource/sonar-examples/blob/master/projects/languages/csharp/sonar-project.properties
	propertiesPath.Dump();
	// http://docs.codehaus.org/display/SONAR/Analysis+Parameters
	var settingLines = new List<string>(){
		"sonar.projectKey=com.clearvoice",
	"sonar.projectName="+projectName,
"sonar.projectVersion=0.1",
// "sonar.sourceEncoding=UTF-8",
"sonar.sources=.",
"sonar.exclusions=obj/**;NonSln/**"
	};
	if(projectDate.HasValue)
		settingLines.Add("sonar.projectDate=" + projectDate.Value.ToString("yyyy-MM-dd"));
	System.IO.File.WriteAllLines(propertiesPath,settingLines.ToArray());
}

public static IEnumerable<ScQueryOutput> TransformScQuery(string output)
{
	
	var grouped=output.SplitLines().SkipWhile (o => string.IsNullOrEmpty(o)).GroupLinesBy("SERVICE_NAME");
	foreach(var line in grouped
				.Select (g => g.SplitLines()
					.Select (l =>l.StringAfterOrSelf(": "))
				.ToArray()))
	{
	
		var serviceName=line[0];
		yield return new ScQueryOutput(){ 
			ServiceName=serviceName,
			DisplayName=line[1], 
			State=line[3]+line[4], 
			Type=line[2],
			Unmapped=line.Skip(4).Delimit(Environment.NewLine) };
	
	}

}

public static IEnumerable<T> AllValues<T>()
	where T:struct
{
	foreach(var item in Enum.GetNames(typeof(T)).Select(v=>(T)Enum.Parse(typeof(T),v)))
	{
		yield return item;
	}
}

#region structs
public struct ScQueryOutput
{
	public string ServiceName{get;set;}
	public string DisplayName{get;set;}
	public string Type{get;set;}
	public string State{get;set;}
	public string Unmapped{get;set;}
}
public struct StreamOuts
{
public string Errors{get;set;}
public string Output{get;set;}
}

#endregion structs
// Define other methods and classes here
public static class EnumerableExtensions
{
//
//public static string Delimit(this IEnumerable<string> values, string delimiter)
//	{
//	return values.Aggregate ((s1,s2)=>s1+delimiter+s2);
//	}
	
	public static IEnumerable<IEnumerable<T>> BufferByCount<T>(this IEnumerable<T> values, int chunkSize)
	{
		var total=0;
var current=values;
		while(current.Any ())
		{
		yield return current.Take(chunkSize);
		total+=chunkSize;
		current=current.Skip(chunkSize);
		}
		yield break;
	}
	
	
	public static IEnumerable<string> GroupLinesBy(this IEnumerable<string> text, string delimiter)
	{
	var sb=new StringBuilder();
	var empties=new StringBuilder();
		foreach(var item in text.SkipWhile (t => string.IsNullOrWhiteSpace(t)))
		{
		if(item.StartsWith(delimiter) && sb.Length>0)
			{
			yield return sb.ToString();
			sb.Clear();
			}
			if(string.IsNullOrWhiteSpace(item))
		empties.AppendLine(item);
		else{
		sb.AppendLine(item);
		empties.Clear();
		}
			
		}
		if(sb.Length>0)
		yield return sb.ToString();
			yield break;
	}
}

public static class StringExtensions
{
public static string RemoveMultipleWhitespaces(this string text)
	{
		return Regex.Replace(text,"\\s\\s+"," ");
	}
public static string TruncateTo(this string text, byte count)
	{
	if(text==null ||text.Length<=count)
	return text;
	return text.Substring(0,count);
	
	}
//public static bool HasValue(this string text)
//	{
//	return string.IsNullOrEmpty(text)==false;
//	}
	
//	public static string[] SplitLines(this string text)
//	{
//		return text.Split(new string[] {"\r\n","\n"}, StringSplitOptions.None);
//	}
	
	public static string StringAfter(this string text, string delimiter)
	{
		return text.Substring( text.IndexOf(delimiter)+delimiter.Length);
	}
	
	public static string StringAfterOrSelf(this string text, string delimiter)
	{
	if(text.Contains(delimiter)==false)
	return text;
	return text.StringAfter(delimiter);
	}
	
	
}

public static class Extensions
{
//
//public static StreamOuts RunProcessRedirected(this Process ps, string arguments)
//	{
//		ps.StartInfo.Arguments=arguments;
//	ps.Start();
//	var output=ps.StandardOutput.ReadtoEndAndDispose();
//	var errors=ps.StandardError.ReadtoEndAndDispose();
//	
//	ps.WaitForExit(2000);
//	if(errors.Length>0) 	Util.Highlight(errors).Dump("errors");
//	return new StreamOuts(){ Errors=errors, Output=output };
//	}
	
	
public static string ReadtoEndAndDispose(this StreamReader reader)
	{
		using(System.IO.StreamReader r=reader)
		{
		return r.ReadToEnd();
		}
	}
	
	
}
// unused but may be useful: http://www.wrightfully.com/setting-up-sonar-analysis-for-c-projects/