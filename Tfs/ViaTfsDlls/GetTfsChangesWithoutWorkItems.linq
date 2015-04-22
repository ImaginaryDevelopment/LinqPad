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
	
	
	var queryPathBase = "$/XpressCharts/";
	
	var tfsServer = Environment.GetEnvironmentVariable("servers").Split(new []{";"},StringSplitOptions.RemoveEmptyEntries).FirstOrDefault(c=>c.Contains("tfs"));
	
	var tfs = new Macros.TFS(tfsServer,"XpressCharts",webport:8080,protocol:"http",path:"tfs"); //Macros.TfsModule.GetTfs(new Uri(tfsUri));
	
		
	var queryPath = GetQueryPath(tfs.TfsPC, useDeeperPath,queryPathBase).Dump("querypath");
	Util.OnDemand("tfs",() => tfs).Dump("tfs");
	//var vcs1 = tfs.TfsPC.GetService<VersionControlServer>();
	//var vcs = Macros.TfsModule.GetVcs(tfs.TfsPC);
	
	var userName = anyUser? null : selectUser? Util.ReadLine("UserName?",Environment.UserName) : tfs.TfsPC.AuthorizedIdentity.UniqueName.AfterOrSelf("\\");
	Util.OnDemand("identity", () =>
	new {
		userName,
		TfsCredentials = tfs.TfsPC.AuthorizedIdentity,
		Environment.UserDomainName,
		Environment.UserName,
		PrincipalIdentity = System.Threading.Thread.CurrentPrincipal.Identity,
		NetworkCredentials = System.Net.CredentialCache.DefaultNetworkCredentials,
		DefaultCredentials = System.Net.CredentialCache.DefaultCredentials,
		TfsUri = tfs.TfsPC.Uri,
		CurrentWinIdentity = System.Security.Principal.WindowsIdentity.GetCurrent(),
		TfsCredentialCache = System.Net.CredentialCache.DefaultNetworkCredentials.GetCredential(tfs.TfsPC.Uri, "Basic")
		
		}).Dump("identity");
	
	// subPath.Dump("subPath");
	var tfsChanges = tfs.GetChangesWithoutWorkItems(userName,queryPath, 0)
		.Dump("all changes")
		;
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