<Query Kind="Statements">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll</Reference>
  <Namespace>Microsoft.TeamFoundation.Client</Namespace>
  <Namespace>Microsoft.TeamFoundation.VersionControl.Client</Namespace>
</Query>

var tfs=new Microsoft.TeamFoundation.Client.TfsTeamProjectCollection(new Uri("https://tfs.oceansideten.com"));
var vcs=tfs.GetService<VersionControlServer>();

vcs.WebServiceLevel.Dump("Tfs info?");
//vcs.GetItems("*.user", RecursionType.Full).Dump();
var bads= new[]{"**.user","**.suo","**resharper**","**.DS_Store","**/packages/**"};
var tp=vcs.GetTeamProject("Development");
//var dev=vcs.GetItem("$/Development");
var badItems=new List<Item>();
foreach(var item in bads){
	var items = vcs.GetItems("$/Development/"+item, RecursionType.Full);
	badItems.AddRange(items.Items);
	items.Items.Select (i => i.ServerItem).Dump();	
}

//http://en.wikipedia.org/wiki/.DS_Store

var myPending= vcs.QueryWorkspaces(null,Environment.UserName,Environment.MachineName).FirstOrDefault ();
if(myPending==null)
return;


foreach(var i in badItems){ //will not delete items that are not mapped into your current workspace
	myPending.PendDelete(i.ServerItem.Dump()).DumpIf(a=>a!=0, "Pend delete error code for "+i.ServerItem);
}
var myChanges=myPending.GetPendingChanges().Select (p => new{p.ChangeType, p.ServerItem});
myChanges
	//.Where (c => c.ChangeType.Has( ChangeType.Delete) )
	.Dump(1);
//http://msdn.microsoft.com/en-us/magazine/jj553516.aspx