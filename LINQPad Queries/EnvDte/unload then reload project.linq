<Query Kind="Program">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 14.0\VSSDK\VisualStudioIntegration\Common\Assemblies\v2.0\Microsoft.VisualStudio.OLE.Interop.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 14.0\VSSDK\VisualStudioIntegration\Common\Assemblies\v4.0\Microsoft.VisualStudio.Shell.14.0.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 14.0\VSSDK\VisualStudioIntegration\Common\Assemblies\v2.0\Microsoft.VisualStudio.Shell.Interop.10.0.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 14.0\VSSDK\VisualStudioIntegration\Common\Assemblies\v2.0\Microsoft.VisualStudio.Shell.Interop.dll</Reference>
  <GACReference>EnvDTE, Version=8.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>EnvDTE</Namespace>
  <Namespace>Microsoft.VisualStudio.Shell</Namespace>
  <Namespace>Microsoft.VisualStudio.Shell.Interop</Namespace>
  <Namespace>System</Namespace>
  <Namespace>System.Security.Principal</Namespace>
</Query>

// unload/reload project
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
	
	var dte = (EnvDTE.DTE)System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE.14.0");
    
    //var typeDte = Type.GetTypeFromProgID("VisualStudio.DTE.12.0");
    // base.GetService(typeof(SVsSolution)) as IVsSolution4;
    var sp = new ServiceProvider((Microsoft.VisualStudio.OLE.Interop.IServiceProvider)dte);
    var sVsSln4 = (IVsSolution4)sp.GetService(typeof(SVsSolution));
    //var sln = ( IVsSolution4) dte.Solution;
    //dte.ActiveSolutionProjects
    var guid = Guid.Parse("95b76f37-4b3c-4530-b5e2-a3910da8a272");
    sVsSln4.UnloadProject(ref guid, (uint) _VSProjectUnloadStatus.UNLOADSTATUS_UnloadedByUser);
    
    System.Threading.Thread.Sleep(1000);
    
    sVsSln4.ReloadProject(ref guid);
	
	
	
}

 public static bool IsAdministrator()
    {
        var identity = WindowsIdentity.GetCurrent();
        var principal = new WindowsPrincipal(identity);
        return principal.IsInRole(WindowsBuiltInRole.Administrator);
    }