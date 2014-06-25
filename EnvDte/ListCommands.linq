<Query Kind="Program">
  <GACReference>EnvDTE, Version=8.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>EnvDTE</Namespace>
  <Namespace>System</Namespace>
</Query>

void Main()
{
	//Visual studio must be running
	var dte = (EnvDTE.DTE)System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE.12.0");
	//var typeDte = Type.GetTypeFromProgID("VisualStudio.DTE.12.0");
	
	//typeDte.Dump();
	
	
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