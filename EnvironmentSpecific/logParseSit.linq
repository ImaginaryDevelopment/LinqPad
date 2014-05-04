<Query Kind="Program" />

void Main()
{
var servers= System.Environment.GetEnvironmentVariable("servers", EnvironmentVariableTarget.User).Split(';').Dump();
var sitFilePath=@"\\"+servers[0]+@"\c$\inetpub\logs\LogFiles";
//var sitFilePath=@"\\svrrbidev03\c$\inetpub\logs\LogFiles";
sitFilePath.Dump();
var hostfilter="cs-host like '%clearvoice%'";
var referrerFilter="cs(Referer) like '%clearvoice%'";
var statusFilter="sc-status not in ('304';'200')";



var currentPath=sitFilePath;

int limit=10;
ParserOutputType? outputType= ParserOutputType.DATAGRID;
var filters=new[]{"("+hostfilter+" or "+referrerFilter+")",statusFilter};
var excludedColumns=new[]{W3CColumns.s_port,W3CColumns.s_ip};
var priorities=new[]{W3CColumns.cs_host, W3CColumns.cs_uri_stem,W3CColumns.cs_uri_query};
var lowPriorities=new[]{W3CColumns.s_computername,W3CColumns.s_sitename, W3CColumns.LogFilename};


bool quietMode=false; //outputType.HasValue && outputType.Value== UserQuery+ParserOutputType.NULL;
var arguments=SetLogParserArguments(outputType,filters,excludedColumns, priorities,lowPriorities,null);
arguments.Dump("arguments");
//var arguments=SetLogParserArguments(outputType,limit,null,null,null,false);

	var logs=from f in System.IO.Directory
			.GetFiles(currentPath,"*.log", SearchOption.AllDirectories)
			let info=new System.IO.FileInfo(f)
			orderby info.LastWriteTimeUtc descending
			select f;
			
			var latest= logs.First();
			latest.Dump("log file");
			var copy=System.IO.Path.GetTempFileName();
			System.IO.File.Copy(latest,copy,true);
			
			var programFiles=System.Environment.GetFolderPath( System.Environment.SpecialFolder.ProgramFiles);
			var filename=System.IO.Path.Combine( programFiles,@"Log Parser 2.2\LogParser.exe");
			if(System.IO.File.Exists(filename)==false)
			{
			("Could not find log parser at "+filename).Dump();
			return;
			}
			//Environment.CurrentDirectory=System.IO.Directory.GetParent(latest);
			
	var outputs=RunProcessRedirected(filename,String.Format(arguments,copy));
	if(outputs.Errors!=null && outputs.Errors.Length>0)
	 Util.Highlight(outputs.Errors).Dump("errors");
	 if(outputs.Output==null||outputs.Output.Length==0)
	 return;
	 var lines=outputs.Output.SplitLines();
	 var resultColumns=lines.First().Dump("Columns");
			
	

} //main

public static string SetLogParserArguments(ParserOutputType? outputType,int? limit=100,
	IEnumerable<string> columns=null,
	string extra=null,
	ParserOptionalBehaviors? flags=null,
	bool checkSyntax=false)
{

var checkSyntaxTxt=(checkSyntax?"-queryinfo":string.Empty);
var top=limit.ValueOrEmpty(a=>" top "+a+" ");
var query=" select "+top+" ";
if(columns!=null)
	query+=columns.Aggregate((s1,s2)=>s1+","+s2);
else query+="*";
query+=" from '{0}' "+extra;

var output=outputType.HasValue?"-o:"+outputType.ToString():string.Empty;//-o:csv
var arguments=checkSyntaxTxt+" -q:on "+output+" \""+query+"\"";
return arguments;
}

public static string SetLogParserArguments(ParserOutputType? outputType,IEnumerable<string> filters, IEnumerable<W3CColumns>blackList, IEnumerable<W3CColumns> priorities,
	IEnumerable<W3CColumns> lowPriorities,int? limit=100,ParserOptionalBehaviors? flags=null)
{
	
		
IDictionary<W3CColumns,string> selectColumns=AllColumnsExcept(blackList);
var orderedColumns=selectColumns
	.OrderByPriority(priorities,f=>f.Key)
		.ThenByPriority(lowPriorities,EnumerableExtensions.Ordering.Descending)
	.Select(c=>c.Value).Dump("columnOrder");
var top=limit.ValueOrEmpty(a=>" top "+a+" ");  //string.Empty;//limit top "+limit+" "
var extra=string.Empty;
if(filters.Any())
extra+=" where "+filters.Aggregate((c1,c2)=>c1+" and "+c2);
extra+=@"order by "+W3CColumns.LogRow +" desc";

return SetLogParserArguments(outputType,limit,orderedColumns,extra,flags);


//query=query.Replace('\r',' ').Replace('\n',' ');


}

public  enum ParserOptionalBehaviors
{
	QuietMode,
	CheckSyntax,
	
}


public static Dictionary<W3CColumns,string> AllColumnsExcept(IEnumerable<W3CColumns> blackList)
{

		var availColumns=MakeW3cDictionary();

 return AllValues<W3CColumns>().ExceptBy(blackList,i=>availColumns[i]).ToDictionary(e=>e,s=>availColumns[s]);
		//.Select(e=>availColumns[e]));
		
		
}

public static IEnumerable<T> AllValues<T>()
	where T:struct
{
	foreach(var item in Enum.GetNames(typeof(T)).Select(v=>(T)Enum.Parse(typeof(T),v)))
	{
		yield return item;
	}
}

public static Dictionary<W3CColumns,string> MakeW3cDictionary()
{
Func<string,string> removeSpecialCharacters= s=>s.RemoveCharacters(new[]{"_","(",")","-"});

var allcolumns=new[]{"LogFilename","LogRow","date","time","c-ip","cs-username","s-sitename","s-computername","s-ip","s-port","cs-method","cs-uri-stem","cs-uri-query","sc-status","sc-substatus","sc-win32-status","sc-bytes","cs-bytes","time-taken","cs-version","cs-host","cs(User-Agent)","cs(Cookie)","cs(Referer)","s-event","s-process-type","s-user-time","s-kernel-time","s-page-faults","s-total-procs","s-active-procs","s-stopped-procs"};

var byEnum=
	from a in allcolumns.OrderBy(f=>f).Select(c=>new{ColumnName=c,Cleaned=removeSpecialCharacters(c)})
	
	join e in Enum.GetNames(typeof(W3CColumns)).Select(e=>new{Column=(W3CColumns) Enum.Parse(typeof(W3CColumns),e),Cleaned=removeSpecialCharacters(e)})
		on a.Cleaned equals e.Cleaned
		select new{e.Column,a.ColumnName};
return byEnum.ToDictionary(a=>a.Column,a=>a.ColumnName);
}

public enum W3CColumns
{/* I */
///LogRow
LogRow,
///s-port
s_port,
///sc-status
sc_status,
///sc-substatus
sc_substatus,
///sc-win32-status
sc_win32_status,
///sc-bytes
sc_bytes,
///cs-bytes
cs_bytes,
///time-taken
time_taken,
///s-page-faults
s_page_faults,
///s-total-procs
s_total_procs,
///s-active-procs
s_active_procs,
///s-stopped-procs
s_stopped_procs,  
/* R */
///s-user-time
s_user_time,
///s-kernel-time
s_kernel_time,  
/* S */
///LogFilename
LogFilename,
///c-ip
c_ip,
///cs-username
cs_username,
///s-sitename
s_sitename,
///s-computername
s_computername,
///s-ip
s_ip,
///cs-method
cs_method,
///cs-uri-stem
cs_uri_stem,
///cs-uri-query
cs_uri_query,
///cs-version
cs_version,
///cs-host
cs_host,
///cs(User-Agent)
cs_User_Agent,
///cs(Cookie)
cs_Cookie,
///cs(Referer)
cs_Referer,
///s-event
s_event,
///s-process-type
s_process_type,  
/* T */
///date
date,
///time
time,  

}

public enum ParserOutputType
{
	CSV, TSV, XML, DATAGRID, CHART, SYSLOG,NEUROVIEW, NAT, W3C, IIS, SQL, TPL, NULL=0
	
}
//public struct StreamOuts
//{
//public string Errors{get;set;}
//public string Output{get;set;}
//}


public static class StringExtensions
{
public static string RemoveCharacters(this string text, IEnumerable<string> items)
	{
	var sb=new System.Text.StringBuilder(text);
	foreach(var item in items)
	{
	var oldVal=sb.ToString();
	sb.Clear();
	sb.Append(oldVal.Replace(item,string.Empty));
	}
	return sb.ToString();
	}
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
public static bool HasValue(this string text)
	{
	return string.IsNullOrEmpty(text)==false;
	}
	
	
	
	
	
	
}
public static class EnumerableExtensions
{
public enum Ordering
	{
	Ascending,
	Descending
	}
	
public static IOrderedEnumerable<T> ThenByPriority<T,TKey>(this IOrderedEnumerable<T> source,
IEnumerable<TKey> priorities,Ordering ordering=Ordering.Ascending)
	{
		var prioritySort=priorities.ToArray();
		if(ordering== Ordering.Ascending)
		return source.ThenBy(s=>Array.IndexOf(prioritySort,s)*-1);
		return source.ThenByDescending(s=>Array.IndexOf(prioritySort,s)*-1);
	}
public static IOrderedEnumerable<T> OrderByPriority<T,TKey>(this IEnumerable<T> source, IEnumerable<TKey> priorities,Func<T,TKey> keySelector,Ordering ordering=Ordering.Ascending)
{
var prioritySort=priorities.ToArray();
return source.OrderBy(s=>Array.IndexOf(prioritySort,keySelector(s))*-1);

}

public static IEnumerable<TSource> ExceptBy<TSource,TKey>(this IEnumerable<TSource> first,IEnumerable<TSource> second,Func<TSource,TKey> keySelector,IEqualityComparer<TKey> keyComparer=null)
	{
		HashSet<TKey> keys=new HashSet<TKey>(second.Select(keySelector),keyComparer);
		foreach(var element in first)
		{
		TKey key= keySelector(element);
		if(keys.Contains(key))
		continue;
		yield return element;
		keys.Add(key);
		}
	}
public static IEnumerable<Tuple<T,int>> WithIndex<T>(this IEnumerable<T> enumerable)
	{
	return enumerable.Select((item,index)=>Tuple.Create(item,index));
	}

public static string Delimit(this IEnumerable<string> values, string delimiter)
	{
	return values.Aggregate ((s1,s2)=>s1+delimiter+s2);
	}
	
}

public static StreamOuts RunProcessRedirected(string filename,string arguments)
{
var psi=new ProcessStartInfo(filename){ RedirectStandardError=true, RedirectStandardOutput=true //,RedirectStandardInput=true 
		, UseShellExecute=false,ErrorDialog=false, CreateNoWindow=true} ; // WinMgmt or WMSvc?

	 
	 using(var ps=new Process(){ StartInfo=psi})
	 {
	
	 return ps.RunProcessRedirected(arguments);
	 }
}
public static class Extensions
{

public static string ValueOrEmpty<T>(this T? nullable, Func<T,string> projection=null)
where T:struct
{
	if(nullable.HasValue==false)
	return String.Empty;
	if(projection==null)
	return nullable.Value.ToString();
	return projection(nullable.Value);
}

//public static IEnumerable<T> ContainedValues<T>(this Enum flagEnum)
//	where T:struct
//{
//if(Enum.IsDefined(typeof(T),
//	return from v in Enum.GetValues(typeof(T)).Cast<T>()
//			where (flagEnum && (Enum)v)==(Enum)v
//			select (T)v;
//}
// Define other methods and classes here
//public static StreamOuts RunProcessRedirected(this Process ps, string arguments)
//		{
//			ps.StartInfo.Arguments=arguments;
//		ps.Start();
//		var output=ps.StandardOutput.ReadtoEndAndDispose();
//		var errors=ps.StandardError.ReadtoEndAndDispose();
//		
//		ps.WaitForExit(2000);
//		if(errors.Length>0) 	Util.Highlight(errors).Dump("errors");
//		return new StreamOuts(){ Errors=errors, Output=output };
//		}
		
		
	public static string ReadtoEndAndDispose(this StreamReader reader)
		{
			using(System.IO.StreamReader r=reader)
			{
			return r.ReadToEnd();
			}
		}
		
}