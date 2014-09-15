<Query Kind="Statements">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll</Reference>
  <GACReference>Microsoft.TeamFoundation.VersionControl.Common, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.TeamFoundation.Client</Namespace>
  <Namespace>Microsoft.TeamFoundation.VersionControl.Client</Namespace>
</Query>

// find check-ins without tickets
var anyUser = false;
var selectUser = false;
var useDeeperPath = false;

var userName =anyUser? null : selectUser? Util.ReadLine("UserName?",Environment.UserName) : Environment.UserName;
var queryPathBase = "$/Development/";

var tfsServer = Environment.GetEnvironmentVariable("servers").Split(new []{";"},StringSplitOptions.RemoveEmptyEntries).FirstOrDefault(c=>c.Contains("tfs"));
var tfsUri = "https://"+tfsServer;
var tfs = new Microsoft.TeamFoundation.Client.TfsTeamProjectCollection(new Uri(tfsUri));
var webPort = 8080;
var teamProject = "Development";
var vcs = tfs.GetService<VersionControlServer>();

var subPaths = vcs.GetItems(queryPathBase, RecursionType.OneLevel).Items.Where(i=>i.ItemType== ItemType.Folder && i.ServerItem.Trim()!= queryPathBase).Select(i=>i.ServerItem.After(queryPathBase)).Dump("subPaths");
var subPath = useDeeperPath? Util.ReadLine("path?",string.Empty,subPaths): string.Empty;
var queryPath = queryPathBase+subPath;

var tp=vcs.GetTeamProject(teamProject);

var webLinkBase = "http://"+tfsServer+":"+webPort+"/DefaultCollection/"+teamProject;

//http://tfs.foo.com:8080/DefaultCollection/Development/_versionControl/changeset/15809#path=%24%2FDevelopment%2FProducts%2FCVS%2FSql%2FCvs.Sql%2Fproject%2FStored+Procedures%2FQuotaGroup_EnableDisableDynamicRecruitmentByQuotaGroupIdList.sql&_a=compare
var itemLinkBase = webLinkBase + "/_versionControl/changeset/{0}#path={1}&_a=compare";

//http://tfs.foo.com:8080/DefaultCollection/Development/_versionControl/changeset/15809
var changeSetLinkBase = webLinkBase + "/_versionControl/changeset/";

// WorkItemCheckedInfo link = 
//http://tfs.foo.com:8080/DefaultCollection/Development/_workitems/edit/5162
var workItemLinkBase = webLinkBase + "/_workitems/edit/";

vcs.QueryHistory(queryPath, VersionSpec.Latest,0, RecursionType.Full,userName,null,null, Int32.MaxValue,true,false)
	.Cast<Changeset>()
	.Where(cs=>cs.AssociatedWorkItems.Length==0 && cs.Changes
		.All(x=>x.Item.ServerItem.Contains("/Playground/"))==false
		)
	.Select(cs=>new{ChangeSetId=new Hyperlinq(changeSetLinkBase+cs.ChangesetId, cs.ChangesetId.ToString()),cs.CreationDate, 
	WorkItems=cs.AssociatedWorkItems.Select(wi=>new{Id=new Hyperlinq(workItemLinkBase+ wi.Id,wi.Id.ToString()),wi.Title,wi.WorkItemType}),
		Changes=cs.Changes.Select(c=>new Hyperlinq(String.Format(itemLinkBase,cs.ChangesetId,System.Net.WebUtility.UrlEncode(c.Item.ServerItem)),c.Item.ServerItem))}).Dump("rollup on changeset/date");