<Query Kind="Program">
</Query>


void Main()
{

 var start="start";
var stop="stop";
var query="query";

var options= new[]{start,stop,query};
var current=Util.ReadLine("Desired action?",query,options);
var servers=System.Environment.GetEnvironmentVariable("servers", EnvironmentVariableTarget.User).Split(';');
	var server=Util.ReadLine("Server?",servers[0],servers);
	
	var psi=new ProcessStartInfo("sc"){ RedirectStandardError=true, RedirectStandardOutput=true //,RedirectStandardInput=true 
		, UseShellExecute=false,ErrorDialog=false, CreateNoWindow=true} ; // WinMgmt or WMSvc?
	 IEnumerable<ScQueryOutput> queryResult;
	 var startOutput=default(My.StreamOuts);
	 bool startHadError=false;
	 string toStart=null;
	using (var ps=new Process())
	{
		ps.StartInfo=psi;
	
	//var input=ps.StandardInput;
	 var queryOutputs=ps.RunProcessRedirected(@"\\" + server+ " query state= all type= service");
	if(queryOutputs.Errors.HasValue()) return;

	queryResult=TransformScQuery(queryOutputs.Output);
	//queryResult.Dump("all");
	var validSvcs=queryResult;
	if(current==start)
		validSvcs=validSvcs.Where(r=>r.State.StartsWith("4")==false);
	else if (current==stop)
		validSvcs=validSvcs.Where(r=>r.State.StartsWith("4"));
	
	 if(validSvcs.Any()==false)
	 {
	 Util.Highlight( "No valid services found to "+current).Dump();
	 queryResult.Select(q=>new{q.DisplayName,State=q.State.RemoveMultipleWhitespaces()}).Dump("Found services on "+server);
	 return;
	 }
	 Util.HorizontalRun(false, validSvcs
	 	.Select (r =>r.ServiceName+"("+r.State.RemoveMultipleWhitespaces().TruncateTo(12)+")").BufferByCount(18)).Dump();
	 toStart=Util.ReadLine(current+" which service?","WMSvc",queryResult.Select (r => r.ServiceName));
	
	if(toStart.HasValue())
		startOutput=ps.RunProcessRedirected(@"\\"+server+" "+current+" "+toStart);
		
	} //ps disposed
	if(startOutput.Errors.HasValue() || startOutput.Output.Contains("FAILED 1060:"))
		startHadError=true;
 	
		Util.ClearResults();
		
	Util.Highlight(startOutput.Output.Trim()).Dump("attempted to "+current+":"+toStart);
	if(toStart.HasValue()==false|| startHadError)
		queryResult.Dump("sc query result");
	
//	 Util.HorizontalRun(false, validSvcs
//	 	.Select (r =>r.ServiceName+"("+r.State.RemoveMultipleWhitespaces().TruncateTo(12)+")").BufferByCount(18)).Dump("services found");
	
	
} //main

public static IEnumerable<ScQueryOutput> TransformScQuery(string output)
{
	output.Dump();
	var grouped=output.SplitLines().SkipWhile (o => string.IsNullOrEmpty(o)).GroupLinesBy("SERVICE_NAME");
	foreach(var line in grouped
				.Select (g => g.SplitLines()
					.Select (l =>l.AfterOrSelf(": "))
				.ToArray()))
	{
	
	var serviceName=line[0];
	serviceName.Dump();
	yield return new ScQueryOutput(){ ServiceName=serviceName,
	 DisplayName=line[1], State=line[3]+line[4], Type=line[2],
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
//public struct StreamOuts
//{
//public string Errors{get;set;}
//public string Output{get;set;}
//}

#endregion structs
// Define other methods and classes here
public static class EnumerableExtensions
{

	
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
		foreach(var item in text.SkipWhile (string.IsNullOrWhiteSpace))
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

public static string TruncateTo(this string text, byte count)
	{
	if(text==null ||text.Length<=count)
	return text;
	return text.Substring(0,count);
	
	}


	
}

public static class Extensions
{

public static string ReadtoEndAndDispose(this StreamReader reader)
	{
		using(System.IO.StreamReader r=reader)
		{
		return r.ReadToEnd();
		}
	}
	
	
}