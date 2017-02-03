<Query Kind="FSharpProgram">
  <GACReference>EnvDTE, Version=8.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>EnvDTE</Namespace>
  <Namespace>System</Namespace>
  <Namespace>System.Security.Principal</Namespace>
</Query>

//Visual studio must be running
//System.Diagnostics.Process.GetProcesses().Select(p=> p.ProcessName).Dump();
let devEnvs = System.Diagnostics.Process.GetProcessesByName "devenv"
let dumpt t x = x.Dump(description=t); x
let isAdministrator () = 
    WindowsIdentity.GetCurrent()
    |> WindowsPrincipal
    |> fun p -> p.IsInRole WindowsBuiltInRole.Administrator

if not <| isAdministrator() && devEnvs.All(fun de -> de.MainWindowTitle.EndsWith "(Administrator)") then
		devEnvs
        |> Seq.map (fun de -> (de.Id, de.MainWindowTitle))
        |> dumpt "Likely will fail, VS appears to be administrator, LinqPad is not"
        |> ignore
	
	
let dte = System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE.14.0") :?> EnvDTE.DTE
	
	// commands to remember
[

		"File.SaveAll",
		"ProjectandSolutionContextMenus.Item.MoveDown",
		"ProjectandSolutionContextMenus.Item.MoveUp",
		"ReSharper_Suspend",
		"ReSharper_ToggleSuspended"
	].Dump("commands of previous interest")
	
dte.FileName.Dump()
dte.Solution.FullName.Dump()
type CommandDisplay = {LocalizedName:string;Id:int;Bindings: obj[]}
let commands=dte.Commands.Cast<Command>().Select (fun c -> {LocalizedName=c.LocalizedName;Id=c.ID;Bindings= c.Bindings :?> obj[]} )

let commandFilter = Util.ReadLine("search term?",null,["resharper";"build";"save"] |> Array.ofList)
let toDisplayCommands=
    if String.IsNullOrEmpty commandFilter then
        commands
    else
	    commands.Where(fun c -> c.LocalizedName.IndexOf(commandFilter,StringComparison.CurrentCultureIgnoreCase) >= 0)
    //|> Seq.filter(fun c -> c.Bindings.Length > 0)		
	
toDisplayCommands.Dump(if not <| isNull commandFilter then commandFilter else "all")
let toRun=Util.ReadLine("Run Command?",null, commands.Select(fun c -> c.LocalizedName))
if String.IsNullOrEmpty(toRun)=false then
	dte.ExecuteCommand(toRun);