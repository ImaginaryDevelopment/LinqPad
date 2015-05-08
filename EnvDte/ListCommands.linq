<Query Kind="Program">
  <GACReference>EnvDTE, Version=8.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>EnvDTE</Namespace>
  <Namespace>System</Namespace>
  <Namespace>System.Security.Principal</Namespace>
</Query>

void Main()
{
	//Visual studio must be running
	//System.Diagnostics.Process.GetProcesses().Select(p=> p.ProcessName).Dump();
	var devEnvs = System.Diagnostics.Process.GetProcessesByName("devenv"); //.Dump();
	if(!IsAdministrator() && devEnvs.All(de => de.MainWindowTitle.EndsWith("(Administrator)"))){
		devEnvs
			.Select(de =>new{de.Id, de.MainWindowTitle})
			//.Select(de=> de.StartInfo)
			.Dump("Likely will fail, VS appears to be administrator, LinqPad is not");
		//return;
	}
	
	var dte = (EnvDTE.DTE)System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE.12.0");
	//var typeDte = Type.GetTypeFromProgID("VisualStudio.DTE.12.0");
	
	//typeDte.Dump();
	
	// commands to remember
	(new [] {
		"ProjectandSolutionContextMenus.Item.MoveDown",
		"ProjectandSolutionContextMenus.Item.MoveUp",
		"ReSharper_Suspend",
		"ReSharper_ToggleSuspended"
	}).Dump("commands of previous interest");
	
	dte.FileName.Dump();
	dte.Solution.FullName.Dump();
	var commands=dte.Commands.Cast<Command>().Select (c =>new{c.LocalizedName,c.ID,Bindings=((object[]) c.Bindings).Cast<string>().ToArray()} );
	var toDisplayCommands=commands;
	var commandFilter = Util.ReadLine("search term?","resharper",new []{"resharper","build","save"});
	if(commandFilter.IsNullOrEmpty()==false)
		toDisplayCommands=commands.Where(c=>c.LocalizedName.IndexOf(commandFilter,StringComparison.CurrentCultureIgnoreCase)>=0);
		
	//var saveCommand = commands.Where(c=>c.LocalizedName.IndexOf("save",StringComparison.CurrentCultureIgnoreCase)>=0).Dump("save commands");
	//var buildCommands=commands.Where (c => c.LocalizedName.IndexOf("build",StringComparison.CurrentCultureIgnoreCase) >=0).Dump("Build commands");
	//var resharper= commands.Where (c => c.LocalizedName.IndexOf("resharper", StringComparison.CurrentCultureIgnoreCase) >=0 //&&  c.Bindings.Any()
	//).Dump("resharper bindings");
	toDisplayCommands.Dump(commandFilter ??"all");
	var toRun=Util.ReadLine("Run Command?",null,commands.Select(c=>c.LocalizedName));
	if(toRun.IsNullOrEmpty()==false)
		dte.ExecuteCommand(toRun);
	
	
}

 public static bool IsAdministrator()
    {
        var identity = WindowsIdentity.GetCurrent();
        var principal = new WindowsPrincipal(identity);
        return principal.IsInRole(WindowsBuiltInRole.Administrator);
    }