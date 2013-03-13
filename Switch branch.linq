<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.Extensions.dll</Reference>
  <Namespace>System.Web.Script.Serialization</Namespace>
  <Namespace>System.Threading.Tasks</Namespace>
</Query>


void Main()
{
	bool useGitUnpushed=true;
	bool doObjBinClean=true;
	bool doBuild=true;
	var junctionDirectory=@"C:\Projects\psh\hpx";
	
	var junctionParent=System.IO.Path.GetDirectoryName(junctionDirectory);
	
	var dirInfo= GetDirInfo(junctionParent);
	dirInfo.Directories.Dump("directories");
	var dirInfos=GetGitInfo(junctionParent, dirInfo.Directories);
	dirInfos.Where (i => i.GitInfo.IsGit).Select (i =>new{i.DirInfo.Name,i.GitInfo.Remotes} ).Dump("GitDirectories");
	var currentTarget=GetJunctionTarget(junctionDirectory,dirInfo.Directories);
	
	var nextTarget=GetUserTarget(currentTarget,junctionParent,dirInfos);
	
	if(nextTarget.IsNullOrEmpty() || nextTarget==System.IO.Path.GetFileName(currentTarget))
	{}
	else
	{
		//new{ nextTarget,currentTarget}.Dump("changing");
		//target is changing
		StopIIS();
		DoChangeJunction(junctionParent,junctionDirectory,nextTarget);
		if(doObjBinClean)
			CleanObjBin(junctionDirectory);
	}
	
	StartIIS();
	
	CheckRemotes(junctionDirectory).DumpOuts( "remotes");
	if(useGitUnpushed)
	CheckUnpushed(junctionDirectory).DumpOuts("unpushed");
	CheckStatus(junctionDirectory).DumpOuts("status");
	var sln=System.IO.Path.Combine(junctionDirectory,"solutions","allapps.sln");
	if(doBuild)
		MsBuild(sln).DumpOuts("built");
}
StreamOuts MsBuild(string path)
{
	return Cmd(@"""""C:\Program Files (x86)\Microsoft Visual Studio 11.0\VC\vcvarsall.bat"" x86 & msbuild """+path+"\"\"");
}
static class Extensions{
	public static StreamOuts DumpOuts(this StreamOuts outs,string message=null)
	{
		return outs.DumpIf(s=>s.Errors.HasValue() || s.Output.HasValue(),message);
	}
	public static T DumpIf<T>(this T source,Func<T,bool> condition,string message=null)
	{
		if(condition(source))
			return source.Dump(message);
		return source;
	}
}

void CleanObjBin(string path)
{
	
	var binResult=CmdAsync(@"FOR /f %i in ('dir /D /S /B BIN') DO del /F /S /Q ""%i\*.*""",path);
	var objResult=	CmdAsync(@"FOR /f %i in ('dir /D /S /B OBJ') DO del /F /S /Q ""%i\*.*""",path);
	Task.WaitAll(binResult,objResult);
	binResult.Result.DumpOuts("bin cleaned");
	objResult.Result.DumpOuts("obj cleaned");
}
IEnumerable<FullInfo> GetGitInfo(string parent,IEnumerable<DirInfo> source)
{
	foreach(var item in source)
	{
		var effectivePath=System.IO.Path.Combine(parent,item.Name);
		if(System.IO.Directory.Exists(System.IO.Path.Combine(effectivePath,".git")))
			yield return new FullInfo {DirInfo=item, GitInfo=new GitInfo(){ IsGit=true, Remotes=CheckRemotes(effectivePath).Output }};
		else
			yield return new FullInfo{ DirInfo=item, GitInfo=new GitInfo{ IsGit=false, Remotes=null}};
	}
}
StreamOuts CheckStatus(string path)
{
	return Git("status",path);
}

StreamOuts CheckUnpushed(string path)
{
	return Git("unpushed",path);
}

StreamOuts CheckRemotes(string path)
{
	return Git("remote -v",path);
}
StreamOuts Git(string args,string workingPath=null)
{
	return RunRedirected("git.exe",args,workingPath);
}
void DoChangeJunction(string parent,string junction, string nextTarget)
{
	
	if(System.IO.Directory.Exists(junction))
		Cmd("rmdir "+junction).Dump("removing junction");
	if(junction==System.IO.Path.Combine(parent,nextTarget))
		throw new InvalidOperationException("Junction can not be to itself");
	new{junction,parent,nextTarget}.Dump("linking");
	Cmd("mklink /j "+junction+" "+nextTarget,parent).Dump("mklink");
}

StreamOuts IIS(string arg)
{
	var output=RunRedirected("iisreset.exe",arg);
	if(output.Errors.HasValue())
		LINQPad.Util.Highlight(output.Errors).Dump("iisreset error");
	return output;
}
void StartIIS(){
	IIS("/start").Dump("StartIIS");
}
void StopIIS(){
	IIS("/stop").Dump("stopping");
	var processes=Process.GetProcessesByName("w3wp.exe");
	processes.Dump("any w3");
	if(processes.Any ())
	{
		processes.Dump("w3wp still running");
		throw new InvalidOperationException("iis did not stop properly");
	}
}

string GetUserTarget(string currentTarget,string parent, IEnumerable<FullInfo> available)
{
	var suggestions=available.Where (d =>d.GitInfo.IsGit &&  d.DirInfo.Target.IsNullOrEmpty() && d.DirInfo.Name!="." && d.DirInfo.Name!=".." && d.DirInfo.Name.IndexOfAny(System.IO.Path.GetInvalidPathChars())==-1)
		.Select (d => d.DirInfo.Name).ToArray();
		if(suggestions.Any ( )==false)
			return null;
	var nextTarget=Util.ReadLine<string>("Target: "+currentTarget+" New Target? "+parent,System.IO.Path.GetFileName(currentTarget),suggestions);
	return nextTarget;
}
async Task<StreamOuts> CmdAsync(string args, string workingDirectory=null)
{
	return await Task.Run(()=>Cmd(args,workingDirectory));
}
StreamOuts Cmd(string args,string workingDirectory=null)
{
	Debug.Assert(args.Contains("/c")==false);
	return RunRedirected("cmd.exe","/c "+args,workingDirectory);
}

string GetJunctionTarget(string junction,IEnumerable<DirInfo> directories)
{
	
		var j=directories.FirstOrDefault(d =>junction.EndsWith(d.Name,StringComparison.CurrentCultureIgnoreCase));
		return j!=null?j.Target:null;
	
}

class DirCmdResult{
	public IEnumerable<DirInfo> Directories{get;set;}
	public string Working{get;set;}
	
}
class FullInfo{
	public GitInfo GitInfo{get;set;}
	public DirInfo DirInfo{get;set;}
	
}
static JavaScriptSerializer jss=new JavaScriptSerializer();
class GitInfo{
	public bool IsGit{get;set;}
	public string Remotes{get;set;}
	override public string ToString()
	{
		return jss.Serialize(this);
	}
}
class DirInfo{
	public DateTime Date{get;set;}
	public string Type{get;set;}
	public string Name{get;set;}
	public string Target{get;set;}
}

StreamOuts RunRedirected(string fileName, string arguments, string workingDirectory=null)
{
	using(var ps=new Process())
	{
	ps.StartInfo.CreateNoWindow=true;
	ps.StartInfo.FileName=fileName;
	if(workingDirectory.HasValue())
		ps.StartInfo.WorkingDirectory=workingDirectory;
	return ps.RunProcessRedirected(arguments);
	}
}

DirCmdResult GetDirInfo(string workingDirectory)
{
	var dirRegex=new Regex(@"([0-9]{2}/[0-9]{2}/[0-9]{4}\s+[0-9]{2}:[0-9]{2} [AP]M)\s+ <([A-Z]+)>\s+(.*)"+Environment.NewLine);
	var output=Cmd("dir",workingDirectory);
	

	return new DirCmdResult(){ Directories=from m in dirRegex.Matches(output.Output).Cast<Match>()
		let dtTime=m.Groups[1].Value
		let nameColumn=m.Groups[3].Value
		let name=nameColumn.BeforeOrSelf(" [")
		let junctionTarget=(nameColumn.Contains(" [")?nameColumn.After(" [").Before("]"):null)
		select new DirInfo{Name=name,Date=DateTime.Parse(dtTime),Type=m.Groups[2].Value, Target=junctionTarget}};
	
	
	
}