<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.Configuration.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Deployment.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.Serialization.Formatters.Soap.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Security.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Namespace>System.Windows.Forms</Namespace>
</Query>

var autocompletes=Environment.GetEnvironmentVariable("servers", EnvironmentVariableTarget.User).Split(';').ToArray();
var m1= Util.ReadLine("Machine 1 name?",autocompletes[0],autocompletes);
var m2= Util.ReadLine("Destination machine name?",autocompletes[0],autocompletes);
var configs=new[]{"machine.config","web.config","app.config"};

var mconfigPath=@"\c$\inetpub\";

var winmerge=@"C:\Program Files (x86)\WinMerge\WinMergeU.exe";
var kdiff3=@"C:\Program Files (x86)\KDiff3\KDiff3.exe";
var diffTool=System.IO.File.Exists(winmerge)?winmerge:kdiff3;

var networkPathBase="\\\\";
//dir compare
var winmergeFormat="/r /e /f *.config /s /u /wl /wr /minimize /dl \"{0}\" /dr \"{1}\" {2} {3}";
var kdiffFormat="-qall --L1 \"{0}\" --L2 \"{1}\" \"{2}\" \"{3}\"";
var format=System.IO.File.Exists(winmerge)?winmergeFormat:kdiffFormat;
string absolutePath;
using(var fbdlg= new FolderBrowserDialog()){ 
	fbdlg.SelectedPath=networkPathBase+m1+mconfigPath;
	if(fbdlg.ShowDialog()!= DialogResult.OK){
		"aborted".Dump();
		return;
	}
	absolutePath=fbdlg.SelectedPath;//.Replace("\\","/");
var m1Info=new { Title=m1==m2?m1+":"+absolutePath.After(m1):m1,Path=absolutePath};

	fbdlg.SelectedPath=networkPathBase+m2+mconfigPath;	

Application.DoEvents();
System.Threading.Thread.Sleep(500);

	if(fbdlg.ShowDialog()!= DialogResult.OK){
		"aborted".Dump();
		return;
	}
	absolutePath=fbdlg.SelectedPath;//.Replace("\\","/");
	var m2Info=new { Title=m2,Path=absolutePath}.Dump("m2");

Process.Start(diffTool,string.Format(format,m1Info.Title,m2Info.Title,m1Info.Path,m2Info.Path));
}