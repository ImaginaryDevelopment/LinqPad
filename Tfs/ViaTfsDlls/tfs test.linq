<Query Kind="Statements">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll</Reference>
  <Namespace>Microsoft.TeamFoundation.Client</Namespace>
  <Namespace>Microsoft.TeamFoundation.VersionControl.Client</Namespace>
</Query>

var tfsServer = Environment.GetEnvironmentVariable("servers").Dump().Split(new []{";"},StringSplitOptions.RemoveEmptyEntries).Dump().FirstOrDefault(c=>c.Contains("tfs"));
var tfsUri= "https://"+tfsServer;
var tfs=new Microsoft.TeamFoundation.Client.TfsTeamProjectCollection(new Uri(tfsUri));

//TeamFoundationServer tfs = TeamFoundationServerFactory.GetServer("https://tfs.oceansideten.com/");
VersionControlServer versionControl = (VersionControlServer)tfs.GetService(typeof(VersionControlServer));
//Workspace ws = versionControl.GetWorkspace(Environment.MachineName, Environment.UserName);

Environment.CurrentDirectory="C:\\Development\\OceansideTen\\OceansideTen.CodeGen\\OceansideTen.CodeGen.UI\\bin\\Debug";
System.IO.File.Exists(@"C:\Development\Products\CVS\Common\CVS.DataAccess\Entities\TextString\DELETEME.CodeGen.cs").Dump();
var filename=@"..\..\..\..\..\Products\CVS\Common\CVS.DataAccess\Entities\TextString\DELETEME.CodeGen.cs";
System.IO.File.Exists(filename).Dump("exists");
var combinedFilename=System.IO.Path.Combine(Environment.CurrentDirectory,filename).Dump("combined");
var fullPath = System.IO.Path.GetFullPath(combinedFilename).Dump("fullpath");
Workspace ws = versionControl.GetWorkspace(fullPath);
string serverItem = ws.GetServerItemForLocalItem(filename);
// if (!string.IsNullOrEmpty(serverItem))
                       // ws.PendEdit(fullPath);
                    //else
                        ws.PendAdd(fullPath);
						
ws.GetPendingChanges().Where(pc=>pc.LocalOrServerItem.Contains("DELETE")).Dump();