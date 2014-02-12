<Query Kind="Program">
  <Reference>C:\Projects\psh\mine\PayspanConfigurationManager\Domain\bin\Debug\Domain.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.Extensions.dll</Reference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>System.Reactive</Namespace>
  <Namespace>System.Reactive.Linq</Namespace>
  <Namespace>System.Threading.Tasks</Namespace>
  <Namespace>System.Web.Script.Serialization</Namespace>
  <Namespace>Domain.Models</Namespace>
  <Namespace>Domain.Adapters</Namespace>
</Query>

void Main()
{
	Domain.Global.SetDump<object>(s=>LINQPad.Extensions.Dump(s));
//todo: check config infos, git unpushed information, git pull master?
//todo: check IIS pointers
	bool useGitUnpushed=true; //unpushed = log --branches --not --remotes --oneline --simplify-by-decoration --decorate
	bool doObjBinClean=false; //clean out the target directory's obj and bin artifacts folders (recursively)
	bool doBuild=false;
	var junctionDirectory=@"C:\Projects\psh\hpx";
	var junction=new Junction(junctionDirectory);
	var sln=System.IO.Path.Combine(junctionDirectory,"solutions","allapps.sln");
	
	var junctionParent=System.IO.Path.GetDirectoryName(junctionDirectory);
	var dirCmd=new DirectoryCmd(junctionParent);
	
	var dirInfo=dirCmd. GetDirInfo();
	dirInfo.Directories.Dump("directories");
	var dirInfos=Git.GetGitInfo(junctionParent, dirInfo.Directories);
	dirInfos.Where (i => i.GitInfo.IsGit).Select (i =>new{i.DirInfo.Name,i.GitInfo.Remotes} ).Dump("GitDirectories");
	var currentTarget=GetJunctionTarget(junctionDirectory,dirInfo.Directories);
	
	var nextTarget=GetUserTarget(currentTarget,junctionParent,dirInfos);
	
	if(nextTarget.IsNullOrEmpty() || nextTarget==System.IO.Path.GetFileName(currentTarget))
	{}
	else
	{
		//new{ nextTarget,currentTarget}.Dump("changing");
		//target is changing
		IIS.ResetStop();
		junction.SetTarget(nextTarget);
		if(doObjBinClean)
			CleanObjBin(junction);
	}
	
	IIS.ResetStart();
	
	Git.CheckRemotes(junctionDirectory).Dump( "remotes");
	if(useGitUnpushed)
		Git.CheckUnpushed(junctionDirectory).Dump("unpushed");
	Git.CheckStatus(junctionDirectory).Dump("status");
	if(doBuild)
		MsBuild(sln);//.DumpOuts("built");
}


void MsBuild(string path)
{
	var msb=new Domain.Adapters.MsBuild("/m:2 \""+path+"\"");
	Domain.Adapters.Process.RunRedirectedObservable(msb);
	
}


static class Extensions{

	
	public static T DumpIf<T>(this T source,Func<T,bool> condition,string message=null)
	{
		if(condition(source))
			return source.Dump(message);
		return source;
	}
}

void CleanObjBin(Junction j)
{
	
var results=j.CleanObjBin();
foreach(var item in results)
item.Dump("cleaned");
}





void StopIIS(){
IIS.ResetStop().Dump();
if(IIS.IsStopped()==false)
	throw new InvalidOperationException("IIS failed to stop");
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

string GetJunctionTarget(string junction,IEnumerable<DirInfo> directories)
{
	
		var j=directories.FirstOrDefault(d =>junction.EndsWith(d.Name,StringComparison.CurrentCultureIgnoreCase));
		return j!=null?j.Target:null;
	
}