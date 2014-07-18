<Query Kind="Statements">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll</Reference>
  <Namespace>Microsoft.TeamFoundation.Client</Namespace>
  <Namespace>Microsoft.TeamFoundation.VersionControl.Client</Namespace>
</Query>

var tfsServer = Environment.GetEnvironmentVariable("servers").Split(new []{";"},StringSplitOptions.RemoveEmptyEntries).FirstOrDefault(c=>c.Contains("tfs"));
var tfsUri= "https://"+tfsServer;
var tfs=new Microsoft.TeamFoundation.Client.TfsTeamProjectCollection(new Uri(tfsUri));
var vcs=tfs.GetService<VersionControlServer>();

var tp=vcs.GetTeamProject("Development");

vcs.QueryHistory("$/Development",VersionSpec.Latest,0, RecursionType.Full,Environment.UserName,null,null, Int32.MaxValue,true,false)
	.Cast<Changeset>()
	.Where(cs=>cs.Changes
		.All(x=>x.Item.ServerItem.Contains("/Playground/"))==false)
	.Select(cs=>new{cs.ChangesetId,cs.CreationDate, WorkItems=cs.AssociatedWorkItems.Select(wi=>new{wi.Id,wi.Title,wi.WorkItemType}),Changes=cs.Changes.Select(c=>c.Item.ServerItem)}).Dump("rollup on changeset/dat\ea");

