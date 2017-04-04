<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>Rstrmgr.Helper</NuGetReference>
  <NuGetReference>Rx-Core</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>Rstrmgr.Helper</Namespace>
  <Namespace>System</Namespace>
  <Namespace>System.Reactive</Namespace>
  <Namespace>System.Reactive.Concurrency</Namespace>
  <Namespace>System.Reactive.Disposables</Namespace>
  <Namespace>System.Reactive.Linq</Namespace>
  <Namespace>System.Reactive.PlatformServices</Namespace>
  <Namespace>System.Reactive.Subjects</Namespace>
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

static bool debug = false;

void Main()
{
	// seems to be working, for more info check out http://stackoverflow.com/questions/317071/how-do-i-find-out-which-process-is-locking-a-file-using-net
	FileUtil.WhoIsLocking(@"C:\Users\Brandon\AppData\Local\Temp\LINQPad5\_lrdgwngc\extensions\BMeffers.dll").Dump("locking");
		
		
} //main