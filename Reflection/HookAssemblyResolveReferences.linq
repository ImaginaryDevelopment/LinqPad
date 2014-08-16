<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Globalization.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>FSharp.Charting</NuGetReference>
  <Namespace>FSharp.Charting</Namespace>
  <Namespace>Microsoft.FSharp.Reflection</Namespace>
  <Namespace>System.Data.OleDb</Namespace>
  <Namespace>System.Windows.Forms</Namespace>
</Query>

//required installation of http://download.microsoft.com/download/2/4/3/24375141-E08D-4803-AB0E-10F2E3A07AAA/AccessDatabaseEngine.exe
//alternative for 64 bit apps : http://download.microsoft.com/download/2/4/3/24375141-E08D-4803-AB0E-10F2E3A07AAA/AccessDatabaseEngine_x64.exe
printfn "starting up"
let RedirectAssembly shortName (targetVersion:Version) publicKeyToken =
	
	let rec onResolveEvent = new ResolveEventHandler( fun sender evArgs ->
		let requestedAssembly = AssemblyName(evArgs.Name)
		if requestedAssembly.Name <> shortName then
			Unchecked.defaultof<Assembly>
		else
			printfn "Redirecting assembly load of %s ,\tloaded by %s" evArgs.Name (if evArgs.RequestingAssembly = null then "(unknown)" else evArgs.RequestingAssembly.FullName)
			requestedAssembly.Version <- targetVersion
			requestedAssembly.SetPublicKeyToken(AssemblyName(sprintf "x, PublicKeyToken=%s" publicKeyToken).GetPublicKeyToken())
			requestedAssembly.CultureInfo <- System.Globalization.CultureInfo.InvariantCulture
			AppDomain.CurrentDomain.remove_AssemblyResolve(onResolveEvent)
			Assembly.Load(requestedAssembly)
			)
	AppDomain.CurrentDomain.add_AssemblyResolve(onResolveEvent)
