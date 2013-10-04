<Query Kind="Program">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE.dll</Reference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>EnvDTE</Namespace>
  <Namespace>System</Namespace>
</Query>

void Main()
{
	//Visual studio must be running
	var dte = (EnvDTE.DTE)System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE");
	dte.FileName.Dump();
	dte.Solution.FullName.Dump();
	var commands=dte.Commands.Cast<Command>().Select (c =>new{c.LocalizedName,c.ID,Bindings=((object[]) c.Bindings).Cast<string>().ToArray()} );

	var buildCommands=commands.Where (c => c.LocalizedName.IndexOf("build",StringComparison.CurrentCultureIgnoreCase) >=0).Dump("Build commands");
	var resharper= commands.Where (c => c.LocalizedName.IndexOf("resharper", StringComparison.CurrentCultureIgnoreCase) >=0 //&&  c.Bindings.Any()
	).Dump("resharper bindings");
}