<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.Threading.dll</Reference>
</Query>

var path=@"C:\Program Files (x86)\Microsoft Visual Studio 10.0\Team Tools\Static Analysis Tools\FxCop\Metrics.exe";
var basePath=Util.ReadLine("Start Where?",@"C:\Projects\trunk\dev-new\src");
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