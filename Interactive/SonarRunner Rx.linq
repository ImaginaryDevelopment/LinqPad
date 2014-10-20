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
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

#define LINQPAD
static bool debug = true;
static bool ignoreLanguageDetectionDebug = true;
static bool tryFxCop = false;
static bool tryStyleCop = false;
void Main()
{
	
	DateTime? backdateForAnalysis = null; //new DateTime(2014,10,2);
	backdateForAnalysis.Dump("backdate");
	var skipPatterns = new [] {"**/*.Tests/**","**/obj/**","NonSln/**","**/Reference.cs","**/*.Designer.cs"};
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
	
	CreateSonarProperties(propertiesPath, skipPatterns, System.IO.Path.GetFileName(slnPath), backdateForAnalysis);
	DateTime.Now.Dump("starting process");
	
	RunSonarRunner(slnDirectory);
	DateTime.Now.Dump("RunSonarRunner finished");	
		
} //main


public static void RunSonarRunner(string slnDirectory)
{
	var psi=new ProcessStartInfo(@"C:\projects\sonar\sonar-runner-2.4\bin\sonar-runner.bat"){ RedirectStandardError=false, RedirectStandardOutput=false
		, UseShellExecute=false,ErrorDialog=false, CreateNoWindow= true,
		Arguments = debug?"-X" : null} ; // WinMgmt or WMSvc?
	int startedProcessId;
	int startedJavaProcessId;
	
	using (var ps=new Process())
	{
		var exited = Observable.FromEventPattern<EventHandler,EventArgs>(
				h => ps.Exited += h,
				h => ps.Exited -= h)
			.Select(e=>{"exited!".Dump(); return true;});
		
	ps.StartInfo=psi;
	
	const string infoMarker = " INFO  - ";
	const string debugMarker = " DEBUG - ";
	
	var output = StandardOutputObservable(ps,exited);
	StandardErrorObservable(ps,exited).Dump("errors");
	
//	errorObservable.Where(data=>data !=null && data.Dump("where data").Contains("is locked by another process")).Do(s=>{
//		var locked = s.Before(" is locked by another process").After("file ").Dump("locked");
//		FileUtil.WhoIsLocking(locked).Dump("locking");
//	});
	var sanitizedOutput= output.Where(s=>s.IsNullOrEmpty() == false && !(s.StartsWith("\t") || s.StartsWith(" ") || s.Contains(" Parse error at line")));
	
	
	
	var nonDebugOutput = sanitizedOutput.Where(s=> s.Contains(debugMarker )==false && s.Contains(infoMarker)==false);
	
	
	
	const string transitionMarker = " source files to be analyzed";
	
	nonDebugOutput 
		.TakeWhile(s=>!s.TrimEnd().EndsWith(transitionMarker))
		.Dump("starting up");
		
		
	sanitizedOutput.Where(s=> s.Contains(infoMarker))
		//.DumpLive("TimedInfo")
		.Dump("TimedInfo");
	
	var debugOutput = sanitizedOutput.Where(s=> s.Contains(debugMarker ) && !ignoreLanguageDetectionDebug || s.Contains("Language of file")==false)
		//.DumpLive("Debug")
		.Dump("debug");	
	
	const string parseFailureMarker = "Unable to parse file:";
	
	nonDebugOutput 
		.SkipWhile(s=> !s.TrimEnd().EndsWith(transitionMarker))
		.Where(s=> s.Contains(parseFailureMarker))
		.Select(parseError => parseError.After(parseFailureMarker).AfterOrSelf(slnDirectory).Trim())
		.Dump("parseErrors");
	
	nonDebugOutput 
		.SkipWhile(s=>!s.TrimEnd().EndsWith(" source files to be analyzed"))
		.Where(s=> !s.Contains(parseFailureMarker))
		.Dump("after startup")
		.Then(_ => {
			"cancelling".Dump();
			if(ps.HasExited==false)
				ps.CancelOutputRead();
			return "finished";
			});
	
	var existingJavas = GetProcInfo(Process.GetProcessesByName("java")).Dump();
	
	new Hyperlinq(()=>GetProcInfo(Process.GetProcessesByName("java")),"check running java processes",true).Dump();
	
	ps.Start();
	startedProcessId = ps.Id.Dump("processId");
	new Hyperlinq(()=>{ 
		if(Process.GetProcesses().Any(running => running.Id == startedProcessId)){
		Process.GetProcessById(startedProcessId).Dump("killing").Kill(); }
		else "Already finished".Dump();
		},"kill process").Dump();
	
	ps.BeginOutputReadLine();
	ps.BeginErrorReadLine();
	
	// wait for the spin up to kick off a java process
	System.Threading.Thread.Sleep(500);
	GetProcInfo(Process.GetProcessesByName("java").Where(p=> existingJavas .Select(j=>j.Id).Contains(p.Id)==false)).Dump("new javas!");
	// ps wait for exit seems to cause the live observable to wait for exit before it appears =(
	ps.WaitForExit();
	
	}	// ps disposed
	
	
}

public static void CreateSonarProperties(string propertiesPath,string[] exclusionPatterns, string projectName, DateTime? projectDate = null)
{ 
	var startupPath = System.IO.Path.GetDirectoryName(propertiesPath);
	// https://github.com/SonarSource/sonar-examples/blob/master/projects/languages/csharp/sonar-project.properties
	propertiesPath.Dump();
	// http://docs.codehaus.org/display/SONAR/Analysis+Parameters
	var keyName = projectName + (tryStyleCop? ".style":string.Empty) + (tryFxCop? ".fx":string.Empty);
	var settingLines = new List<string>(){
		"sonar.projectKey="+keyName,
		"sonar.projectName="+keyName,
		"sonar.projectVersion=0.1",
		"sonar.sourceEncoding=UTF-8",
		"sonar.sources=.",
		"sonar.exclusions=" + string.Join(",", exclusionPatterns),
		"sonar.language=cs",
		"sonar.dotnet.visualstudio.solution.file="+projectName,
		
	};
	if(!tryStyleCop){
		settingLines.Add("sonar.stylecop.projectFilePath=");
		settingLines.Add("sonar.stylecop.mode=skip");
		
	} else {
		var includedProjects = from projectfile in System.IO.Directory.GetFiles(startupPath, "*.csproj", SearchOption.AllDirectories)
					where exclusionPatterns.Any(ep => projectfile.IsMatch(ep.Replace("**",".*").Replace("*.","[^\\\\]*").Replace("/","\\\\"),true))==false
					select projectfile.Replace("\\","/");
		settingLines.Add("sonar.stylecop.projectFilePath="+string.Join(",", includedProjects));
		
		// one per project?
		//"sonar.stylecop.projectFilePath="+projectName
	}
	
	if (!tryFxCop) {
		settingLines.Add("sonar.fxcop.mode=skip");
	}
	
	if(projectDate.HasValue)
		settingLines.Add("sonar.projectDate=" + projectDate.Value.ToString("yyyy-MM-dd"));
		
	settingLines.Dump("settings");
	System.IO.File.WriteAllLines(propertiesPath,settingLines.ToArray());
}


public static IObservable<string> StandardErrorObservable (Process process, IObservable<bool> exited){
	process.EnableRaisingEvents = true;
	process.StartInfo.RedirectStandardError = true;
	
	var recieved = Observable.FromEventPattern<DataReceivedEventHandler, DataReceivedEventArgs>(
		handler => handler.Invoke,
		h => process.ErrorDataReceived += h,
		h => process.ErrorDataReceived -= h)
	.Select( e =>e.EventArgs.Data)
	.TakeUntil(exited);
	recieved.Then(s=> {"exited event!".Dump(); return s;});
	
	//return recieved;
	// handle cancellation or closure?
	return Observable.Create<string>(observer =>
	{
		var cancel = Disposable.Create(process.CancelOutputRead);
		return new CompositeDisposable(cancel, recieved.Subscribe(observer));
	});

}

public static IObservable<string> StandardOutputObservable (Process process, IObservable<bool> exited){
	process.EnableRaisingEvents = true;
	process.StartInfo.RedirectStandardOutput = true;
	var recieved = Observable.FromEventPattern<DataReceivedEventHandler, DataReceivedEventArgs>(
		handler => handler.Invoke,
		h => process.OutputDataReceived += h,
		h => process.OutputDataReceived -= h)
	.Select( e =>e.EventArgs.Data)
	.TakeUntil(exited);
	recieved.Then(s=> {"exited event!".Dump(); return s;});
	return recieved;
	// handle cancellation or closure?
//	return Observable.Create<string>(observer =>
//	{
//		var cancel = Disposable.Create(process.CancelOutputRead);
//		return new CompositeDisposable(cancel, recieved.Subscribe(observer));
//	});
	
}

static IEnumerable<ProcInfo> GetProcInfo(IEnumerable<Process> processes){
	foreach(var p in processes.OrderByDescending(x=>x.StartTime))
		yield return new ProcInfo(){ 
			ProcessName=p.ProcessName, 
			StartTime = p.StartTime, 
			Responding = p.Responding, 
			Id =p.Id, 
			HasExited = p.HasExited, 
			EnableRaisingEvents = p.EnableRaisingEvents,
			Kill = new Hyperlinq(() =>Process.GetProcessById(p.Id).Kill(),"Kill " + p.Id,true)
			};
}

class ProcInfo {
	public string ProcessName{get;set;}
	public DateTime StartTime {get;set;}
	public bool Responding {get;set;}
	public int Id {get;set;}
	public bool HasExited{get;set;}
	public bool EnableRaisingEvents{get;set;}
	public Hyperlinq Kill {get;set;}
	
}
	

// unused but may be useful: http://www.wrightfully.com/setting-up-sonar-analysis-for-c-projects/