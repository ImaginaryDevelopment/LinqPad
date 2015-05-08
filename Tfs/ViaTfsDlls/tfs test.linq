<Query Kind="Statements">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll</Reference>
  <Reference>C:\projects\Fsi\tfsmacros.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.TeamFoundation.Client</Namespace>
  <Namespace>Microsoft.TeamFoundation.VersionControl.Client</Namespace>
</Query>

var tfs = tfsMacros.getTfs();

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