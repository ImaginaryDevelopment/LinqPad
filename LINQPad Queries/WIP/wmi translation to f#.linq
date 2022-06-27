<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Management.dll</Reference>
  <NuGetReference>System.Management</NuGetReference>
  <Namespace>System.Globalization</Namespace>
  <Namespace>System.Management</Namespace>
</Query>

// make this into a disk space checker for laptop, vm, and maybe network drives
// works 2022.06.26

type System.String with
    member x.After(delimiter:string) =
        let i = x.IndexOf(delimiter) + delimiter.Length
        x.[ i .. ]
    member x.Before(delimiter:string) =
        x.[0.. x.IndexOf(delimiter) - 1]
    member x.AfterLast(delimiter:string) =
        let i = x.LastIndexOf(delimiter) + delimiter.Length
        x.[ i .. ]
let (* string *) memoryToUI(number: UInt32) =
    if number > 2048u then
        number.ToString("n0")
    else
        let mb = number / 1024u
        mb.ToString("n0") + "Kb"
       
let (* string *) bigMemoryToUI(number: UInt64) =
    let kb = number / 1024UL
    if kb < 2000UL then
        kb.ToString("n0")+"Kb"
    else 
        let mb = kb / 1024UL
        mb.ToString("n0") + "Mb"
//http://aspalliance.com/806_CodeSnip_Get_memory_size_usage_using_WMI_and_NET_20
let autocompletes =
    (Environment.GetEnvironmentVariable("servers", EnvironmentVariableTarget.User)
    |> Option.ofObj
    |> Option.defaultValue Environment.MachineName
    ).Split(';').ToArray()
let cpuName = Util.ReadLine("Server?",autocompletes.[0],autocompletes)
cpuName.Dump("querying")

let scope = new System.Management.ManagementScope(@"\\" + cpuName + @"\root\cimv2")
let useSearcher query f =
    let q = new SelectQuery(query)
    let searcher' = new ManagementObjectSearcher(scope,q)
    use searcher = searcher'.Get()
    f searcher
type Win32ProcessInfo = {
    ProcessId: string
    Name: string
    ThreadCount: string
    // maybe only relevant for iis/w3wp.exe?
    Config: string
    CreationDate: DateTime
    PageFileUsage: string
    PeakPageFileUsage: string
    VirtualSize: string
    PeakVirtualSize: string
    PeakWorkingSetSize: string
    PrivatePageCount: string
    InstallDate: string
}
let getWin32Processes nameOpt =
    //http://msdn.microsoft.com/en-us/library/windows/desktop/aa394372(v=vs.85).aspx
    let query =
        let baseQ = "select * FROM Win32_Process"
        // for instance w3wp.exe
        match nameOpt with
        | Some n ->
            sprintf "%s WHERE name='%s'" baseQ n
        | None -> baseQ
    useSearcher query (fun searcher ->
        let items=searcher.Cast<ManagementObject>()
        let props=items.Select (fun i -> i.Properties)
        let _cleanIISCommandLine (x:string) =
            x.After("-h").After("\"").Before("\"")
        let cleanCommandLine (x:string) =
            match x with
            | null -> None
            | x ->
                if x.StartsWith("\"") then
                    Some(x.After("\"").Before("\""), x.After("\"").After("\""))
                else Some(x.Before(" "), x.After(" "))
            
        let  getProp (pdc:PropertyDataCollection) (k:string) : string = if isNull pdc.[k].Value then "null" else string pdc.[k].Value
        props
        |> Seq.map(fun p -> 
            let name, config =
                    getProp p "CommandLine"
                    |> cleanCommandLine
                    |> Option.defaultValue (null,null)
            {
                ProcessId= getProp p "ProcessId"
                ThreadCount= getProp p "ThreadCount"
                Name= name
                    //getProp p "CommandLine"
                    //|> cleanCommandLine
                    //|> fun v -> v.AfterLast("\\").Before(".")
                Config= config
                CreationDate=ManagementDateTimeConverter.ToDateTime(getProp p "CreationDate")
                PageFileUsage= memoryToUI(UInt32.Parse(getProp p "PageFileUsage"))
                PeakPageFileUsage=memoryToUI(UInt32.Parse( getProp p "PeakPageFileUsage"))
                VirtualSize=bigMemoryToUI(UInt64.Parse(getProp p "VirtualSize"))
                PeakVirtualSize=bigMemoryToUI(UInt64.Parse( getProp p "PeakVirtualSize"))
                PeakWorkingSetSize=memoryToUI(UInt32.Parse(getProp p "PeakWorkingSetSize"))
                PrivatePageCount=bigMemoryToUI(UInt64.Parse( getProp p "PrivatePageCount"))
                InstallDate= getProp p "InstallDate"
            }
        )
        |> fun x ->  x.Dump(depth=0,collapseTo=Nullable 1)
        |> ignore
)
getWin32Processes None
    
module GeneralInfo = 
    let getMemory() = 
        let query = "select Name,Roles,NumberOfProcessors,NumberOfLogicalProcessors,TotalPhysicalMemory from "+ "win32_computersystem"
        useSearcher query (fun searcher ->
        	let items=
                searcher
                    .Cast<ManagementObject>()
            		.Select (fun mo -> 
                            mo.Properties
                    			.Cast<PropertyData>()
                    			.Select (fun pd -> {| Name= pd.Name;Value=pd.Value;Type=pd.Type;IsArray=pd.IsArray |}))
        	let mem = UInt64.Parse( items.First().First(fun i -> i.Name = "TotalPhysicalMemory").Value.ToString())
        	bigMemoryToUI(mem).Dump("TotalPhysicalMemory")
    	)
    getMemory()
    
    let getMemInfo () =
        let query =
            "select FreePhysicalMemory,FreeSpaceInPagingFiles,FreeVirtualMemory,SizeStoredInPagingFiles,TotalVirtualMemorySize,TotalVisibleMemorySize from "
            + "win32_operatingsystem"
        
        useSearcher query (fun searcher ->
            let items =
                searcher 
                    .Cast<ManagementObject>()
                    .Select (fun mo ->
                        mo.Properties
                            .Cast<PropertyData>()
                            .Select (fun pd -> {| Name=pd.Name;Value=bigMemoryToUI(UInt64.Parse(pd.Value.ToString()));Raw=pd.Value;Type=pd.Type;IsArray=pd.IsArray |}))
            
            items.Dump(query.After(" from "))
        )
    getMemInfo()
    
//http://msdn.microsoft.com/en-us/library/windows/desktop/aa394261(v=vs.85).aspx
let getPerfDisk() =
    let query = "select * from win32_perfrawdata_perfdisk_logicaldisk"
    useSearcher query (fun searcher ->
        let items=
            searcher
                .Cast<ManagementObject>()
                .Select(fun mo ->
                    mo.Properties
                        .Cast<PropertyData>()
                        .Select (fun pd -> {| Name=pd.Name;Value=pd.Value; Type=pd.Type;IsArray=pd.IsArray |})
                )
                .OrderBy(fun mo ->
                    mo
                    |> Seq.tryFind(fun pd -> pd.Name="Name")
                    |> Option.map(fun pd -> pd.Value)
                )
                .GroupBy(fun mo ->
                    mo
                    |> Seq.tryFind(fun pd -> pd.Name="Name")
                    |> Option.map(fun pd -> pd.Value)
                )
        
        items.Dump(query.After(" from "));
    )
getPerfDisk()
//class Win32_Process{
//	string   Caption;
//  string   CommandLine;
//  string   CreationClassName;
//  DateTime CreationDate;
//  string   CSCreationClassName;
//  string   CSName;
//  string   Description;
//  string   ExecutablePath;
//  UInt16   ExecutionState;
//  string   Handle;
//  UInt32   HandleCount;
//  DateTime InstallDate;
//  UInt64   KernelModeTime;
//  UInt32   MaximumWorkingSetSize;
//  UInt32   MinimumWorkingSetSize;
//  string   Name;
//  string   OSCreationClassName;
//  string   OSName;
//  UInt64   OtherOperationCount;
//  UInt64   OtherTransferCount;
//  UInt32   PageFaults;
//  UInt32   PageFileUsage;
//  UInt32   ParentProcessId;
//  UInt32   PeakPageFileUsage;
//  UInt64   PeakVirtualSize;
//  UInt32   PeakWorkingSetSize;
//  UInt32   Priority;
//  UInt64   PrivatePageCount;
//  UInt32   ProcessId;
//  UInt32   QuotaNonPagedPoolUsage;
//  UInt32   QuotaPagedPoolUsage;
//  UInt32   QuotaPeakNonPagedPoolUsage;
//  UInt32   QuotaPeakPagedPoolUsage;
//  UInt64   ReadOperationCount;
//  UInt64   ReadTransferCount;
//  UInt32   SessionId;
//  string   Status;
//  DateTime TerminationDate;
//  UInt32   ThreadCount;
//  UInt64   UserModeTime;
//  UInt64   VirtualSize;
//  string   WindowsVersion;
//  UInt64   WorkingSetSize;
//  UInt64   WriteOperationCount;
//  UInt64   WriteTransferCount;
//}

