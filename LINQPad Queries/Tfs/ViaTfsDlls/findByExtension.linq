<Query Kind="Statements">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll</Reference>
  <Reference Relative="..\..\..\..\Fsi\tfsmacros.dll">C:\projects\Fsi\tfsmacros.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.TeamFoundation.Client</Namespace>
  <Namespace>Microsoft.TeamFoundation.VersionControl.Client</Namespace>
  <Namespace>Macros</Namespace>
</Query>

var tfs = new{ Tfs = Macros.TfsModule.GetTfs(new Uri("http://tfs20102:8080/tfs"))};

//tfs.Dump();

var vcs = TfsModule.GetVcs(tfs.Tfs);
vcs.WebServiceLevel.Dump("Tfs info?");

//vcs.GetItems("*.user", RecursionType.Full).Dump();
var bads= new[]{
	"**.bat",
	// , "**/packages/**" 
	};
	
var targetPath = "$/";
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
	items.Items.Select (i => i.ServerItem).DumpIf(x=>x.Any(),item);	
}



var myPending= vcs.QueryWorkspaces(null,Environment.UserName,Environment.MachineName).FirstOrDefault ();
if(myPending==null)
return;

myPending.Dump("checking workspace for changes",1);
