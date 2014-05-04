<Query Kind="Statements" />

var autocompletes=Environment.GetEnvironmentVariable("servers", EnvironmentVariableTarget.User).Split(';').ToArray();
var m1= Util.ReadLine("Machine 1 name?",autocompletes[0],autocompletes);
var m2= Util.ReadLine("Machine 2 name?",autocompletes[1],autocompletes.Except(new[]{m1}).ToArray());
var configs=new[]{"machine.config","web.config","app.config"};
var mconfigPath=@"\c$\Windows\Microsoft.NET\Framework\v4.0.30319\Config\";
var winmerge=@"C:\Program Files (x86)\WinMerge\WinMergeU.exe";
var kdiff3=@"C:\Program Files (x86)\KDiff3\KDiff3.exe";
var diffTool=System.IO.File.Exists(winmerge)?winmerge:kdiff3;

var networkPathBase="\\\\";
var winmergeFormat="/e /s /u /wl /wr /dl \"{0}\" /dr \"{1}\" {2} {3}";
var kdiffFormat="-qall --L1 \"{0}\" --L2 \"{1}\" {2} {3}";
var format=System.IO.File.Exists(winmerge)?winmergeFormat:kdiffFormat;
foreach(var config in configs)
{
	var m1Info=new { Title=m1+"-"+config, Path=networkPathBase+m1+mconfigPath+config};
	var m2Info=new { Title=m2+"-"+config, Path=networkPathBase+m2+mconfigPath+config};

if(System.IO.File.Exists(networkPathBase+m1+mconfigPath+config))
	Process.Start(diffTool,string.Format(format,m1Info.Title,m2Info.Title,m1Info.Path,m2Info.Path));
}