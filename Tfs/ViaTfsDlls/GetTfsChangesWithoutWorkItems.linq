<Query Kind="Program">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll</Reference>
  <Reference>C:\projects\Fsi\tfsmacros.dll</Reference>
  <GACReference>Microsoft.TeamFoundation.VersionControl.Common, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.TeamFoundation.Client</Namespace>
  <Namespace>Microsoft.TeamFoundation.VersionControl.Client</Namespace>
  <Namespace>Macros</Namespace>
</Query>

void Main()
{
	// find check-ins without tickets
	var anyUser = false;
	var selectUser = false;
	var useDeeperPath = false;
	
	var userName =anyUser? null : selectUser? Util.ReadLine("UserName?",Environment.UserName) : Environment.UserName;
	var queryPathBase = "$/Development/";
	
	var tfsServer = Environment.GetEnvironmentVariable("servers").Split(new []{";"},StringSplitOptions.RemoveEmptyEntries).FirstOrDefault(c=>c.Contains("tfs"));
	var tfsUri = "https://"+tfsServer;
	
	var tfs = new TFS(tfsServer,8080,"Development");
	
	var queryPath = GetQueryPath(tfs.Tfs, useDeeperPath,queryPathBase);
	
	// subPath.Dump("subPath");
	var tfsChanges = tfs.GetChangesWithoutWorkItems(userName,queryPath, 0);
	// tfsChanges.Dump();
	
	tfsChanges
		.Select(cs=>new{ cs.Owner,
			ChangeSetId=new Hyperlinq(tfs.GetChangesetLink(cs.ChangesetId), cs.ChangesetId.ToString()),
			cs.CreationDate, 
			Changes=cs.Changes.Select(c=>new Hyperlinq(tfs.GetItemLink(cs.ChangesetId,System.Net.WebUtility.UrlEncode(c)),c))
		}).Dump("rollup on changeset/date");
}

// Define other methods and classes here
public string GetQueryPath(TfsTeamProjectCollection tfs, bool useDeeperPath,string queryPathBase){
	if(!useDeeperPath) return queryPathBase;
	var vcs = tfs.GetService<VersionControlServer>();
	var subPaths = vcs.GetItems(queryPathBase, RecursionType.OneLevel).Items.Where(i=>i.ItemType== ItemType.Folder && i.ServerItem.Trim()!= queryPathBase).Select(i=>i.ServerItem.After(queryPathBase)).Dump("subPaths");
	var subPath = useDeeperPath? Util.ReadLine("path?",string.Empty,subPaths): string.Empty;
	return queryPathBase+subPath;
}