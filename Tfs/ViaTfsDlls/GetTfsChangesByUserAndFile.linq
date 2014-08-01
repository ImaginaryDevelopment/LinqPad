<Query Kind="Statements">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll</Reference>
  <Namespace>Microsoft.TeamFoundation.Client</Namespace>
  <Namespace>Microsoft.TeamFoundation.VersionControl.Client</Namespace>
</Query>

var tfsServer = Environment.GetEnvironmentVariable("servers").Split(new []{";"},StringSplitOptions.RemoveEmptyEntries).FirstOrDefault(c=>c.Contains("tfs"));
var tfsUri= "https://"+tfsServer;
var tfs=new Microsoft.TeamFoundation.Client.TfsTeamProjectCollection(new Uri(tfsUri));

var webPort=8080;
var teamProject="Development";
var vcs=tfs.GetService<VersionControlServer>();

var tp=vcs.GetTeamProject(teamProject);

var webLinkBase = "http://"+tfsServer+":"+webPort+"/DefaultCollection/"+teamProject;
var itemLinkBase = webLinkBase + "/_versionControl/changeset/{0}#path={1}&_a=compare";

//http://tfs.foo.com:8080/DefaultCollection/Development/_versionControl/changeset/15809
var changeSetLinkBase = webLinkBase + "/_versionControl/changeset/";

// WorkItemCheckedInfo link = 
//http://tfs.foo.com:8080/DefaultCollection/Development/_workitems/edit/5162
var workItemLinkBase = webLinkBase + "/_workitems/edit/";

vcs.QueryHistory("$/Development",VersionSpec.Latest,0, RecursionType.Full,Environment.UserName,null,null, Int32.MaxValue,true,false)
	.Cast<Changeset>()
	.Where(cs=>cs.Changes
		.All(x=>x.Item.ServerItem.Contains("/Playground/"))==false)
	.Select(cs=>new{ChangeSetId=new Hyperlinq(changeSetLinkBase+cs.ChangesetId, cs.ChangesetId.ToString()),cs.CreationDate, 
	WorkItems=cs.AssociatedWorkItems.Select(wi=>new{Id=new Hyperlinq(workItemLinkBase+ wi.Id,wi.Id.ToString()),wi.Title,wi.WorkItemType}),
		Changes=cs.Changes.Select(c=>new Hyperlinq(String.Format(itemLinkBase,cs.ChangesetId,System.Net.WebUtility.UrlEncode(c.Item.ServerItem)),c.Item.ServerItem))}).Dump("rollup on changeset/date");