<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>Rx-Core</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>System</Namespace>
  <Namespace>System.Reactive</Namespace>
  <Namespace>System.Reactive.Concurrency</Namespace>
  <Namespace>System.Reactive.Disposables</Namespace>
  <Namespace>System.Reactive.Linq</Namespace>
  <Namespace>System.Reactive.PlatformServices</Namespace>
  <Namespace>System.Reactive.Subjects</Namespace>
</Query>

void Main()
{

DateTime? backdateForAnalysis = new DateTime(2014,4,1);
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

	CreateSonarProperties(propertiesPath, System.IO.Path.GetFileName(slnPath),backdateForAnalysis);

	RunSonarRunner(slnDirectory);
	
} //main

public static IObservable<string> StandardOutputObservable (Process process){
	process.EnableRaisingEvents = true;
	process.StartInfo.RedirectStandardOutput = true;
	var recieved = Observable.FromEventPattern<DataReceivedEventHandler, DataReceivedEventArgs>(
	handler => handler.Invoke,
	h => process.OutputDataReceived += h,
	h => process.OutputDataReceived -= h)
	.Select( e => e.EventArgs.Data);
	
	//return recieved;
	// handle cancellation or closure?
	return Observable.Create<string>(observer =>
	{
		var cancel = Disposable.Create(process.CancelOutputRead);
		return new CompositeDisposable(cancel, recieved.Subscribe(observer));
	});
	
}

public static void RunSonarRunner(string slnDirectory)
{
	var psi=new ProcessStartInfo(@"C:\projects\sonar\sonar-runner-2.4\bin\sonar-runner.bat"){ RedirectStandardError=false, RedirectStandardOutput=false
		, UseShellExecute=false,ErrorDialog=false, CreateNoWindow= true} ; // WinMgmt or WMSvc?

	using (var ps=new Process())
	{
	ps.StartInfo=psi;
	var soObs = StandardOutputObservable(ps).DumpLive("sonarRunner output");
	
	ps.Start();
	ps.BeginOutputReadLine();
	ps.WaitForExit();
	
	}	//ps disposed
	
	
}

public static void CreateSonarProperties(string propertiesPath, string projectName, DateTime? projectDate = null)
{ 
	// https://github.com/SonarSource/sonar-examples/blob/master/projects/languages/csharp/sonar-project.properties
	propertiesPath.Dump();
	// http://docs.codehaus.org/display/SONAR/Analysis+Parameters
	var settingLines = new List<string>(){
		"sonar.projectKey=com.cvs",
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


	

// unused but may be useful: http://www.wrightfully.com/setting-up-sonar-analysis-for-c-projects/