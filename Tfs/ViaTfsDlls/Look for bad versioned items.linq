<Query Kind="Statements">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll</Reference>
  <Reference>C:\projects\Fsi\tfsmacros.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.TeamFoundation.Client</Namespace>
  <Namespace>Microsoft.TeamFoundation.VersionControl.Client</Namespace>
  <Namespace>Macros</Namespace>
</Query>

var tfs = new{ Tfs = Macros.TfsModule.GetTfs(new Uri("http://tfs2010:8080/tfs"))};

//tfs.Dump();

var vcs = TfsModule.GetVcs(tfs.Tfs);
vcs.WebServiceLevel.Dump("Tfs info?");

//vcs.GetItems("*.user", RecursionType.Full).Dump();
var bads= new[]{
	"**.user",
	"**.suo",
	"**resharper**",
	"**.DS_Store", //http://en.wikipedia.org/wiki/.DS_Store
	// , "**/packages/**" 
	};
	
var targetPath = "$/XpressCharts/Source-Development/";
//check the directory path for validity, throws if not found
try
{	        
	vcs.GetItem(targetPath);	
}
catch (Exception ex)
{
	vcs.GetItems("$/*").Items.Select(i => i.ServerItem).Dump("are valid paths");
	ex.Dump();
	return;
}

var badItems=new List<Item>();
foreach(var item in bads){
	var items = vcs.GetItems(targetPath+item, RecursionType.Full);
	badItems.AddRange(items.Items);
	items.Items.Select (i => i.ServerItem).DumpIf(x=>x.Any(),item+ " is bad");	
}



var myPending= vcs.QueryWorkspaces(null,Environment.UserName,Environment.MachineName).FirstOrDefault ();
if(myPending==null)
return;

myPending.Dump("checking workspace for changes",1);

foreach(var i in badItems){ //will not delete items that are not mapped into your current workspace
	myPending.PendDelete(i.ServerItem.Dump()).DumpIf(a=>a!=0, "Pend delete error code for "+i.ServerItem);
}
var myChanges=myPending.GetPendingChanges().Select (p => new{p.ChangeType, p.ServerItem});
myChanges
	//.Where (c => c.ChangeType.Has( ChangeType.Delete) )
	.Dump("pendingchanges",1);
//http://msdn.microsoft.com/en-us/magazine/jj553516.aspx