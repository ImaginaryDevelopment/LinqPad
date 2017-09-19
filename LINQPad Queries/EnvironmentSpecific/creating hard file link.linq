<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Xaml.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\WindowsBase.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationCore.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Configuration.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\UIAutomationProvider.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\UIAutomationTypes.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\ReachFramework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\PresentationUI.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\WPF\System.Printing.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Accessibility.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Deployment.dll</Reference>
  <Namespace>Microsoft.Win32</Namespace>
</Query>

// hard link a file in windows
//const string OPENDIR_CACHE = nameof(OPENDIR_CACHE);

// can't use var because of nameof()
string lastOpenDir = (string)AppDomain.CurrentDomain.GetData(nameof(lastOpenDir));
string lastTargetDir = (string)AppDomain.CurrentDomain.GetData(nameof(lastTargetDir)); 

new { LastFromDir = lastOpenDir, LastTargetDir = lastTargetDir}.Dump();

var ofd = new OpenFileDialog();

if (string.IsNullOrEmpty(lastOpenDir) == false)
    ofd.InitialDirectory = lastOpenDir;
    
var dfd = new SaveFileDialog();
if (!ofd.ShowDialog().GetValueOrDefault())
	return;
    
var sourceFileName = ofd.SafeFileName.Dump("linking from");

// cache the dir the selected file is in
AppDomain.CurrentDomain.SetData(nameof(lastOpenDir),Path.GetDirectoryName(ofd.FileName));

if (string.IsNullOrEmpty(lastTargetDir) == false)
{
    var projectedTargetFileName = Path.Combine(lastTargetDir,sourceFileName).Dump("projected target"); 
    dfd.InitialDirectory = lastTargetDir;
    dfd.FileName = sourceFileName;
}
else
    dfd.FileName = sourceFileName;

if(!dfd.ShowDialog().GetValueOrDefault())
	return;
	
if (File.Exists(dfd.FileName))
{
    // the dialog is confirming you want to overwrite... so don't fail (is the safety prompt only happening on windows 10?)
    
	dfd.FileName.Dump("File already exists, failing");
	return;
}

LINQPad.Util.Cmd("mklink /h \""+ dfd.FileName +"\" \"" + ofd.FileName + "\"");
if (!File.Exists(dfd.FileName))
    dfd.FileName.Dump("Command failed to create a link at");
else
{
    lastTargetDir = Path.GetDirectoryName(dfd.FileName).Dump("setting lastTargetDir");
    AppDomain.CurrentDomain.SetData(nameof(lastTargetDir), lastTargetDir);
}