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
var ofd = new OpenFileDialog();
var dfd = new SaveFileDialog();
if (!ofd.ShowDialog().GetValueOrDefault())
	return;
ofd.SafeFileName.Dump("linking to");
if(!dfd.ShowDialog().GetValueOrDefault())
	return;
	
if (File.Exists(dfd.FileName))
{
	dfd.FileName.Dump("File already exists, failing");
	return;
}

LINQPad.Util.Cmd("mklink /h \""+ dfd.FileName +"\" \"" + ofd.FileName + "\"");
if(!File.Exists(dfd.FileName))
	dfd.FileName.Dump("Command failed to create a link at");

