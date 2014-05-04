<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.Threading.dll</Reference>
</Query>

var path=@"C:\Program Files (x86)\Microsoft Visual Studio 12.0\Team Tools\Static Analysis Tools\FxCop\Metrics.exe";
if(System.IO.Directory.Exists(System.IO.Path.GetDirectoryName(path))==false){
	System.IO.Path.GetDirectoryName(path).Dump("not found, aborting");
	return;	
}
if(System.IO.File.Exists(path)==false){
	path.Dump("not found, aborting");
	return;
}
var basePath=Util.ReadLine("Start Where?",@"%devroot%");
if(basePath.Contains("%"))
	basePath=System.Environment.ExpandEnvironmentVariables(basePath);
	//if the variables don't expand take the more in depth approach
if(basePath.Contains("%"))
{
	foreach(var toReplace in Regex.Matches(basePath,"%(.*)%").Cast<Match>().Select (m =>new{m.Value,inner= m.Groups[1].Value }))
	basePath=basePath.Replace(toReplace.Value,System.Environment.GetEnvironmentVariable(toReplace.inner.Dump("inner"), EnvironmentVariableTarget.Machine)??System.Environment.GetEnvironmentVariable(toReplace.inner, EnvironmentVariableTarget.Process)?? System.Environment.GetEnvironmentVariable(toReplace.inner, EnvironmentVariableTarget.User));

}
var startInfo=new ProcessStartInfo(path){ CreateNoWindow=true, UseShellExecute=false,RedirectStandardError=true, RedirectStandardOutput=true};
var outputPath=System.IO.Path.Combine(System.Environment.GetFolderPath( System.Environment.SpecialFolder.Desktop),
	"measures");
	var currentOutputs=new HashSet<string>();
using(var process=new Process())
{
	process.StartInfo=startInfo;
	foreach(var dll in System.IO.Directory.GetFiles(basePath,"*.dll", SearchOption.AllDirectories)
		.Where (d =>d.Contains("Microsoft")==false ))
	{
	
		var outputTarget=System.IO.Path.Combine(outputPath,System.IO.Path.GetFileName(dll)+".xml");
		if(currentOutputs.Contains(outputTarget))
			continue;
		currentOutputs.Add(outputTarget);
		("Processing "+dll).Dump();
		//process.StartInfo.Arguments=
		
		process.RunProcessRedirected("/gac /f:"+dll+" /o:"+outputTarget);
		
		process.Start();
		process.WaitForExit(3000);
		var output=new System.IO.FileInfo( outputTarget);
		if(output.Length<1)
		{
			System.IO.File.Delete(outputTarget);
			currentOutputs.Remove(outputTarget);
		}
	}
}