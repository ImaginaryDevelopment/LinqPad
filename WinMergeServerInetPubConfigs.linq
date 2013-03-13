<Query Kind="Statements">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE.dll</Reference>
</Query>

var autocompletes=new[]{"hn2web1","pqoweb1","jaxpdrweb1"};
var m1= Util.ReadLine("Machine 1 name?","pqoweb1",autocompletes);
var m2= Util.ReadLine("Machine 2 name?","jaxpdrweb1",autocompletes);
var configs=new[]{"machine.config","web.config","app.config"};
var mconfigPath=@"\e$\payspan\inetpub";
var winmerge=@"C:\Program Files (x86)\WinMerge\WinMergeU.exe";
var networkPathBase="\\\\";
var winmergeFormat="/r /e /f *.config /s /u /wl /wr /minimize /dl \"{0}\" /dr \"{1}\" {2} {3}";

	var m1Info=new { Title=m1,Path=networkPathBase+m1+mconfigPath};
	var m2Info=new { Title=m2,Path=networkPathBase+m2+mconfigPath};

	Process.Start(winmerge,string.Format(winmergeFormat,m1Info.Title,m2Info.Title,m1Info.Path,m2Info.Path));