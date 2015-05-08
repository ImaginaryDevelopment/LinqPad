<Query Kind="Program" />

void Main()
{

	var storage=new Dictionary<string,Tuple<ChangeBreadcrumb,ChangeBreadcrumb>>();
	
	var mappings=new List<Tuple<string,string>>(){
		Tuple.Create(@"$/PST","$/PSA"),
		};
	var tfs08="http://tfs.foo.com:8080/";
	var tfs10="http://g-tfs.foo.com:8080/tfs/";
	
	foreach(var mapping in mappings)
	{
	var history10=GetLastHistory(tfs10,mapping.Item2);
	
	if(string.IsNullOrEmpty(history10.Item2)==false)
	{
	("tfs10 error:"+mapping.Item2).Dump();
	continue;
	}
	history10.Item1.Dump("tfs10");
	var history08=GetLastHistory(tfs08,mapping.Item1);
	history08.Item1.Dump();
	if(string.IsNullOrEmpty(history08.Item2)==false)
	{
	("tfs08error:"+mapping.Item1).Dump();
	}
	continue;
	
	}
	
	
}
public class ChangeBreadcrumb
{
public DateTime Date{get;set;}
public string User{get;set;}
public string Path{get;set;}
}
public static Tuple<string,string> GetLastHistory( string collectionUrl,string path)
{
Tuple<string,string> result=null;
using (var process=new Process())
	{
	process.StartInfo.UseShellExecute=false;
	process.StartInfo.FileName=@"C:\Program Files\Microsoft Visual Studio 10.0\Common7\IDE\tf.exe";
	process.StartInfo.RedirectStandardError=true;
	process.StartInfo.RedirectStandardOutput=true;
	process.StartInfo.Arguments=" history /recursive /stopafter:1 /i /collection:"+collectionUrl+" \""+path+"\"";
	
	process.Start();
	var errors=process.StandardError.ReadToEndAndDispose();
	var output=process.StandardOutput.ReadToEndAndDispose();
	process.WaitForExit();
	result=Tuple.Create(output,errors);
	}
	
	return result;
}
// Define other methods and classes here
public static class Extensions
{
public static string ReadToEndAndDispose(this StreamReader reader)
{
using(System.IO.StreamReader r=reader)
return r.ReadToEnd();
}
}