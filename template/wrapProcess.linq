<Query Kind="Program" />

void Main()
{
var filename="sc";
	var psi=new ProcessStartInfo(filename){ RedirectStandardError=true, RedirectStandardOutput=true //,RedirectStandardInput=true 
		, UseShellExecute=false,ErrorDialog=false, CreateNoWindow=true} ; // WinMgmt or WMSvc?
	
	 StreamOuts startOutput=default(StreamOuts);
	 using(var ps=new Process(){ StartInfo=psi})
	 {
	 
	 }
}

public struct StreamOuts
{
public string Errors{get;set;}
public string Output{get;set;}
}

public static class Extensions
{
// Define other methods and classes here
public static StreamOuts RunProcessRedirected(this Process ps, string arguments)
		{
			ps.StartInfo.Arguments=arguments;
		ps.Start();
		var output=ps.StandardOutput.ReadtoEndAndDispose();
		var errors=ps.StandardError.ReadtoEndAndDispose();
		
		ps.WaitForExit(2000);
		if(errors.Length>0) 	Util.Highlight(errors).Dump("errors");
		return new StreamOuts(){ Errors=errors, Output=output };
		}
		
		
	public static string ReadtoEndAndDispose(this StreamReader reader)
		{
			using(System.IO.StreamReader r=reader)
			{
			return r.ReadToEnd();
			}
		}
		
}