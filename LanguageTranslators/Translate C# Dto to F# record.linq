<Query Kind="FSharpExpression">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Namespace>System.Web</Namespace>
</Query>

//C# to sql
//var input=LINQPad.Util.ReadLine<string>("What shall we reformat?");
let input ="""{
string   Caption;
  string   CommandLine;
  string   CreationClassName;
  DateTime CreationDate;
  string   CSCreationClassName;
  string   CSName;
  string   Description;
  string   ExecutablePath;
  UInt16   ExecutionState;
  string   Handle;
  UInt32   HandleCount;
  DateTime InstallDate;
  UInt64   KernelModeTime;
  UInt32   MaximumWorkingSetSize;
  UInt32   MinimumWorkingSetSize;
  string   Name;
  string   OSCreationClassName;
  string   OSName;
  UInt64   OtherOperationCount;
  UInt64   OtherTransferCount;
  UInt32   PageFaults;
  UInt32   PageFileUsage;
  UInt32   ParentProcessId;
  UInt32   PeakPageFileUsage;
  UInt64   PeakVirtualSize;
  UInt32   PeakWorkingSetSize;
  UInt32   Priority;
  UInt64   PrivatePageCount;
  UInt32   ProcessId;
  UInt32   QuotaNonPagedPoolUsage;
  UInt32   QuotaPagedPoolUsage;
  UInt32   QuotaPeakNonPagedPoolUsage;
  UInt32   QuotaPeakPagedPoolUsage;
  UInt64   ReadOperationCount;
  UInt64   ReadTransferCount;
  UInt32   SessionId;
  string   Status;
  DateTime TerminationDate;
  UInt32   ThreadCount;
  UInt64   UserModeTime;
  UInt64   VirtualSize;
  string   WindowsVersion;
  UInt64   WorkingSetSize;
  UInt64   WriteOperationCount;
  UInt64   WriteTransferCount;
}"""
[
for l in input.SplitLines() do 
	if l.Contains(";") then 
		let itemType = l.Trim().Before(" ").Trim()
		let name = l.Trim().After(" ").Trim().Before(";")
		yield name+":"+itemType
]	
|> fun l -> String.Join("\r\n",l)