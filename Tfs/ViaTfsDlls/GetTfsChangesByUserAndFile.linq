<Query Kind="Program">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll</Reference>
  <Reference>C:\projects\Fsi\tfsmacros.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.TeamFoundation.Client</Namespace>
  <Namespace>Microsoft.TeamFoundation.VersionControl.Client</Namespace>
</Query>

void Main()
{
	var selectUser = false;
	var useDeeperPath = false;
	
	var userName = selectUser? Util.ReadLine("UserName?",Environment.UserName) : Environment.UserName;
	var tfsServer = Environment.GetEnvironmentVariable("servers").Split(new []{";"},StringSplitOptions.RemoveEmptyEntries).FirstOrDefault(c=>c.Contains("tfs"));
	var tfsUri = "https://"+tfsServer;
	var teamProject = "Development";
	
	// tfs is needed to let user provide a subPath if desired
	var tfs = new Microsoft.TeamFoundation.Client.TfsTeamProjectCollection(new Uri(tfsUri));
	
	var queryPathBase = "$/"+teamProject+"/";
	var queryPath = GetQueryPath(tfs, useDeeperPath,queryPathBase);
	
	var tfsChanges = tfsMacros.CSharp.getTfsChangesByUserAndFile(tfs, userName,queryPath, 0);
	var items = tfsChanges.Item2;
	items
		.Select(cs=>new{ChangeSetId=new Hyperlinq(tfsMacros.getChangesetLink(cs.ChangesetId), cs.ChangesetId.ToString()),cs.CreationDate, 
		WorkItems=cs.AssociatedWorkItems.Select(wi=>new{Id=new Hyperlinq(tfsMacros.getWorkItemLink(wi.Item1),wi.Item1.ToString()),wi.Item2,wi.Item3}),
			Changes=cs.Changes.Select(c=>new Hyperlinq(tfsMacros.getItemLink(cs.ChangesetId,System.Net.WebUtility.UrlEncode(c)),c))}).Dump("rollup on changeset/date");
}

// Define other methods and classes here
public string GetQueryPath(TfsTeamProjectCollection tfs, bool useDeeperPath,string queryPathBase){
	if(!useDeeperPath) return queryPathBase;
	var vcs = tfs.GetService<VersionControlServer>();
	var subPaths = vcs.GetItems(queryPathBase, RecursionType.OneLevel).Items.Where(i=>i.ItemType== ItemType.Folder && i.ServerItem.Trim()!= queryPathBase).Select(i=>i.ServerItem.After(queryPathBase)).Dump("subPaths");
	var subPath = useDeeperPath? Util.ReadLine("path?",string.Empty,subPaths): string.Empty;
	return queryPathBase+subPath;
}