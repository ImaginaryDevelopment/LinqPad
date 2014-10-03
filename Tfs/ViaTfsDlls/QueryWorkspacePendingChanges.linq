<Query Kind="Statements">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll</Reference>
  <Reference>C:\projects\Fsi\tfsmacros.dll</Reference>
  <GACReference>Microsoft.TeamFoundation.VersionControl.Client, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Macros</Namespace>
  <Namespace>Microsoft.TeamFoundation.Client</Namespace>
  <Namespace>Microsoft.TeamFoundation.VersionControl.Client</Namespace>
</Query>

var tfsServer = Macros.TfsModule.GetTfsServerFromEnvironment();

var tfs =new Macros.TFS(tfsServer,"Development", null);
var vcs= tfs.Tfs.GetService<Microsoft.TeamFoundation.VersionControl.Client.VersionControlServer>();
var searchBase=vcs.GetItem("$/");
//vcs.Dump();
var workspace = vcs.GetWorkspace(Environment.ExpandEnvironmentVariables("%devroot%"));

// vcs.QueryWorkspaces(null,null,null).Dump("workspaces");
var itemSpecs = new []{new ItemSpec(searchBase.ServerItem,RecursionType.Full)};

vcs.QueryPendingSets(itemSpecs, workspace.Name, workspace.OwnerName, false, true)
	.Select(ps=>new {Pending=ps.PendingChanges.Select(pc=>new{ pc.LocalOrServerItem,PendingChange=Util.OnDemand("PendingChange",()=> pc)}),Candidate = ps.CandidatePendingChanges})
	.First()
	.Dump();
	
workspace.Dump();