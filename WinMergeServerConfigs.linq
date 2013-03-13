<Query Kind="Statements">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE.dll</Reference>
</Query>

var autocompletes=new[]{"hn2web1","pqoweb1","jaxpdrweb1"};
var m1= Util.ReadLine("Machine 1 name?","pqoweb1",autocompletes);
var m2= Util.ReadLine("Machine 2 name?","jaxpdrweb1",autocompletes);
var configs=new[]{"machine.config","web.config","app.config"};
var mconfigPath=@"\c$\Windows\Microsoft.NET\Framework\v4.0.30319\Config\";
var winmerge=@"C:\Program Files (x86)\WinMerge\WinMergeU.exe";
var networkPathBase="\\\\";
var winmergeFormat="/e /s /u /wl /wr /dl \"{0}\" /dr \"{1}\" {2} {3}";

foreach(var config in configs)
{
	var m1Info=new { Title=m1+"-"+config, Path=networkPathBase+m1+mconfigPath+config};
	var m2Info=new { Title=m2+"-"+config, Path=networkPathBase+m2+mconfigPath+config};

if(System.IO.File.Exists(networkPathBase+m1+mconfigPath+config))
	Process.Start(winmerge,string.Format(winmergeFormat,m1Info.Title,m2Info.Title,m1Info.Path,m2Info.Path));
}