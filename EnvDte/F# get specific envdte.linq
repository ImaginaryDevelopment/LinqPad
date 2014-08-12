<Query Kind="FSharpProgram">
  <GACReference>EnvDTE, Version=8.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
</Query>

// http://www.fssnip.net/raw/68
module vs10macros =
	open EnvDTE
	open System.Runtime.InteropServices 
	open System.Runtime.InteropServices.ComTypes
	open System.Diagnostics

	[<DllImport("ole32.dll")>] 
	extern int internal GetRunningObjectTable(uint32 reserved, IRunningObjectTable& pprot) 
 
	[<DllImport("ole32.dll")>] 
	extern int internal CreateBindCtx(uint32 reserved, IBindCtx& pctx) 
	
	let getWindowNames = 
		let dvproc = Process.GetProcessesByName("devenv")
		let p = Array.map (fun (px:Process) -> px.MainWindowTitle) dvproc
		p
	let getPidByWindowName (wn:string) =
		let dvproc = Process.GetProcessesByName("devenv")
		let p = Array.tryFind (fun (px:Process) -> 
				px.MainWindowTitle.ToLower().Contains(wn.ToLower())) dvproc
		match p with
		|Some x -> x.Id
		|None -> failwith "No process contains a window with this title"
		 
    let getDte windowName  = 
       let mutable (prot:IRunningObjectTable) = null  
       let mutable (pmonkenum:IEnumMoniker) = null 
       let (monikers:IMoniker[]) =  Array.create 1 null 
       let pfeteched = System.IntPtr.Zero 
       let mutable (ret:obj) = null 
       let pid = getPidByWindowName windowName
       let endpid = sprintf ":%d" pid 
       try
           if (GetRunningObjectTable(0u, &prot) <> 0) || (prot = null) then  
               failwith "Error opening the ROT" 
           prot.EnumRunning(&pmonkenum) 
           pmonkenum.Reset() 
           while pmonkenum.Next(1, monikers, pfeteched) = 0 do 
               let mutable (insname:string) = null 
               let mutable (pctx:IBindCtx) = null 
               CreateBindCtx(0u, &pctx) |> ignore 
               (monikers.[0]).GetDisplayName(pctx, null, &insname); 
               Marshal.ReleaseComObject(pctx) |> ignore 
               if insname.StartsWith("!VisualStudio.DTE") && insname.EndsWith(endpid) then 
                   prot.GetObject(monikers.[0], &ret) |> ignore 
       finally 
           if prot <> null then Marshal.ReleaseComObject(prot) |> ignore 
           if pmonkenum <> null then Marshal.ReleaseComObject(pmonkenum) |> ignore 
       (ret :?> EnvDTE.DTE)

let dteTitles = vs10macros.getWindowNames
let window = 
	match dteTitles.Length with
	| 0 -> failwithf "Failed to find any DevEnv instances running"
	| 1 -> dteTitles.[0]
	| _ ->
		dteTitles.Dump("windows")
		printfn "%A" dteTitles
		Util.ReadLine("Dte?",dteTitles.[0],dteTitles)
let dte = vs10macros.getDte window
printfn "%A" dte
printfn "%A" dte.FileName
printfn "%A" dte.Solution.FullName