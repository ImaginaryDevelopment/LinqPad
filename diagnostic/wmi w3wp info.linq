<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Management.dll</Reference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>System.Management</Namespace>
  <Namespace>System.Globalization</Namespace>
</Query>

void Main()
{
	//http://aspalliance.com/806_CodeSnip_Get_memory_size_usage_using_WMI_and_NET_20
	
	var cpuName=Util.ReadLine("Server?","pqoweb1");
	cpuName.Dump("querying");
	//http://msdn.microsoft.com/en-us/library/windows/desktop/aa394372(v=vs.85).aspx
	var query= "select * FROM Win32_Process WHERE name='w3wp.exe'";
	//var culture=System.Globalization.CultureInfo.CurrentUICulture;
	//culture.NumberFormat.Dump();
	var scope =new System.Management.ManagementScope(@"\\"+cpuName+@"\root\cimv2");
	var q= new SelectQuery(query);
	//scope.Connect();
	var searcher = new ManagementObjectSearcher(scope,q);
	//scope.Connect();
	
	
	using(var disposable=searcher.Get())
	{
		var items=disposable.Cast<ManagementObject>();
		var props=items.Select (i => i.Properties);
		Func<string,string> cleanCommandLine = i=> i.After("-h").After("\"").Before("\"");//.AfterLast("\\");
		Func<PropertyDataCollection,string,string> Prop=(pdc,k) => 
			pdc[k].Value != null ?
				pdc[k].Value.ToString() :
				"null";
		props.Select (p => 
			new{
				ProcessId=Prop(p,"ProcessId"),
				ThreadCount=Prop(p,"ThreadCount"),
				Name=cleanCommandLine(Prop(p,"CommandLine")).AfterLast("\\").Before("."),
				Config= cleanCommandLine(Prop(p,"CommandLine")),
				CreationDate=ManagementDateTimeConverter.ToDateTime(Prop(p,"CreationDate")),
				PageFileUsage=MemoryToUI(UInt32.Parse(Prop(p,"PageFileUsage"))),
				PeakPageFileUsage=MemoryToUI(UInt32.Parse( Prop(p,"PeakPageFileUsage"))),
				VirtualSize=MemoryToUI(UInt64.Parse(Prop(p,"VirtualSize"))),
				PeakVirtualSize=MemoryToUI(UInt64.Parse( Prop(p,"PeakVirtualSize"))),
				PeakWorkingSetSize=MemoryToUI(UInt32.Parse(Prop(p,"PeakWorkingSetSize"))),
				PrivatePageCount=MemoryToUI(UInt64.Parse( Prop(p,"PrivatePageCount"))),
				InstallDate= Prop(p,"InstallDate"),
				
			}).Dump();
		//props.Skip(1).First ().Dump("Sample w3wp props");
		
		
	}
	q.QueryString=
	"select Name,Roles,NumberOfProcessors,NumberOfLogicalProcessors,TotalPhysicalMemory from "+
		"win32_computersystem";
	using(var disposable=new ManagementObjectSearcher(scope,q).Get())
	{
		var items=disposable
			.Cast<ManagementObject>()
			.Select (mo => mo.Properties
				.Cast<PropertyData>()
				.Select (pd => new{ pd.Name,pd.Value,pd.Type,pd.IsArray}));
		//items.Dump(q.QueryString.After(" from "));
		MemoryToUI( UInt64.Parse( items.First ().First (i => i.Name=="TotalPhysicalMemory").Value.ToString())).Dump("TotalPhysicalMemory");
	}
	
	q.QueryString="select FreePhysicalMemory,FreeSpaceInPagingFiles,FreeVirtualMemory,SizeStoredInPagingFiles,TotalVirtualMemorySize,TotalVisibleMemorySize from "+
		"win32_operatingsystem";
	using(var disposable=new ManagementObjectSearcher(scope,q).Get())
	{
		var items=disposable
			.Cast<ManagementObject>()
			.Select (mo => mo.Properties
				.Cast<PropertyData>()
				.Select (pd => new{ pd.Name,Value=MemoryToUI(UInt64.Parse(pd.Value.ToString())),Raw=pd.Value,pd.Type,pd.IsArray}));
		
		items.Dump(q.QueryString.After(" from "));
	}
	//http://msdn.microsoft.com/en-us/library/windows/desktop/aa394261(v=vs.85).aspx
	q.QueryString="select * from win32_perfrawdata_perfdisk_logicaldisk";
	using(var disposable=new ManagementObjectSearcher(scope,q).Get())
	{
		var items=disposable
			.Cast<ManagementObject>()
			.Select (mo => mo.Properties
				.Cast<PropertyData>()
				.Select (pd => new{ pd.Name,Value=pd.Value,pd.Type,pd.IsArray}));
		
		items.Dump(q.QueryString.After(" from "));
	}
}
class Win32_Process{
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
}

string MemoryToUI(UInt32 number)
{
	if(number>2048)
		return number.ToString("n0");
	var mb=number /1024;
	return mb.ToString("n0")+"Kb";
}
string MemoryToUI(UInt64 number)
{
	var kb=number /1024;
	if(kb<2000)
	return kb.ToString("n0")+"Kb";
	var mb= kb / 1024;
	return mb.ToString("n0")+"Mb";
}