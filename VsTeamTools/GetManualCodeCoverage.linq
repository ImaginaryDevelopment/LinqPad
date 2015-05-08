<Query Kind="FSharpProgram" />

// http://stackoverflow.com/questions/4958776/batch-running-of-vs-code-coverage-tools
// 2010 version: http://generichuman4897.blogspot.com/2010/12/enabling-code-coverage-in-vs2010.html
// 2010 version2: http://scissortools.wordpress.com/2011/12/03/code-coverage-test-in-visual-studio-2010-things-to-be-aware-of/
let vsInstrPath = """C:\Program Files (x86)\Microsoft Visual Studio 12.0\Team Tools\Performance Tools\vsinstr.exe"""
let vsPerfCmdPath = """C:\Program Files (x86)\Microsoft Visual Studio 12.0\Team Tools\Performance Tools\VSPerfCmd.exe"""
module helpers =
	type applicationHostSite = { Name:string; Path:string; Physical:string; Protocol: string; Pool:string;Binding:string; Element:XElement}
	type applicationHostSite with
		member this.Uri = 
			LINQPad.Hyperlinq(this.Protocol + "://" + this.Binding.Split([|":"|],StringSplitOptions.None).Skip(1).Reverse().Delimit(":") )
		member this.Directory = match this.Physical with
								| x when System.IO.Directory.Exists(x) -> LINQPad.Hyperlinq(x)
								| _ -> null
	let ns s= XNamespace.None+s
	type System.Xml.Linq.XElement with
		member el.Element(s:string) = el.Element(ns s)
		member el.Elements(s:string) = el.Elements(ns s)
		member el.GetAttribValOpt(s:string) = match el.Attribute(ns s) with
												| a when a <> null -> Some(a.Value)
												| _ -> None
		member el.GetAttribValOrNull(s:string) = match el.Attribute(ns s) with
												| a when a <> null -> a.Value
												| _ -> null
open helpers	
let appHostConfigPath = Environment.ExpandEnvironmentVariables("""%homedrive%%homepath%\Documents\IISExpress\config\applicationhost.config""")
let xDoc = XDocument.Load(appHostConfigPath)
//sitesFile.Document.Root.Elements("system.applicationHost") |> Dump
let sites = 
	let siteElements =	xDoc.Root
							.Element("system.applicationHost")
							.Element("sites")
							.Elements("site")
	let sitesWithExpecteds = siteElements |>
							Seq.filter (fun e->
								let app = e.Element("application")
								app <> null &&
								app.Element("virtualDirectory") <> null)
	sitesWithExpecteds |> Seq.map (fun e-> 
						let app = e.Element("application")
						let virt = app.Element("virtualDirectory")
						let binding = e.Element("bindings").Element("binding")
						{
							Name =e.GetAttribValOrNull("name");
							Path= virt.GetAttribValOrNull("path");
							Physical=virt.GetAttribValOrNull("physicalPath");
							Element= e;
							Pool = app.GetAttribValOrNull("applicationPool");
							Protocol = binding.GetAttribValOrNull("protocol");
							Binding = binding.GetAttribValOrNull("bindingInformation")
						})
	
//siteElements |> Dump

sites.Dump("sites to choose from in "+appHostConfigPath)
let siteNames = [ for i in sites do yield i.Name]
let selectedSiteName = Util.ReadLine("Which site?",String.Empty, siteNames)
let selectedSite =sites |> Seq.find(fun e ->  e.Name = selectedSiteName)
selectedSite.Dump("selected")
let coverageFileName = "run.coverage"
//helpers 

let guard f (e:IEvent<'Del, 'Args>) = // http://stackoverflow.com/a/2658530/57883
    let e = Event.map id e
    { new IEvent<'Args> with 
        member x.AddHandler(d) = e.AddHandler(d); f() //must call f here!
        member x.RemoveHandler(d) = e.RemoveHandler(d)
        member x.Subscribe(observer) = 
          let rm = e.Subscribe(observer) in f(); rm }
		  
//http://stackoverflow.com/questions/3065409/starting-a-process-synchronously-and-streaming-the-output
//http://stackoverflow.com/questions/2649161/need-help-regarding-async-and-fsi
		  
//http://stackoverflow.com/questions/3065409/starting-a-process-synchronously-and-streaming-the-output
let runProcessAsync (fileName:string,args:string) = async {
	printfn "starting %s" args
	let psi = new System.Diagnostics.ProcessStartInfo(fileName,args,UseShellExecute=false, RedirectStandardOutput=true, RedirectStandardError=true, CreateNoWindow=true)
	use p = new System.Diagnostics.Process(StartInfo = psi)
	let output = new System.Text.StringBuilder()
	let error = new System.Text.StringBuilder()
	p.OutputDataReceived.Add(fun args -> output.Append(args.Data) |> ignore)
	p.ErrorDataReceived.Add(fun args -> error.Append(args.Data) |> ignore)
	p.Start() |> ignore
    p.BeginErrorReadLine()
    p.BeginOutputReadLine()
    let! exit =
		p.Exited
		|> guard (fun _ -> p.EnableRaisingEvents <- true)
		|> Async.AwaitEvent
	return ( p.ExitCode,output.ToString(),error.ToString())
	}
let command (s:string) =
	let psi = new System.Diagnostics.ProcessStartInfo("cmd","/c"+s)
	psi.UseShellExecute <- false
	let p = System.Diagnostics.Process.Start(psi)
	p.WaitForExit();
	p.ExitCode

let baseDir=Util.ReadLine("App Directory?",System.Environment.GetEnvironmentVariable("devroot"));

//build the app (if not built)
//TODO: build the app?
// get the output path for instrumentation
let binDir=Util.ReadLine("bin Directory?",System.IO.Path.Combine( baseDir, "Products\\CVS\\Manage\\CVS.Manage.Web\\bin"))
Environment.CurrentDirectory <- binDir
match System.IO.File.Exists(coverageFileName) with 
	| true -> System.IO.File.Delete(coverageFileName)
	| _ -> printfn "no existing coverage file to delete"
	
//instrument  ******************************************************************************************
// VsInstr http://msdn.microsoft.com/en-us/library/ms182402.aspx
let dlls = System.IO.Directory.GetFiles(binDir, "*.dll")
let filteredDlls = dlls |> Seq.filter (fun x -> 
	let fileName=System.IO.Path.GetFileName(x)
	fileName.Contains("CVS") || fileName.Contains("Oceanside"))
//filteredDlls |> Dump
let tasks = seq { 
	for dll in filteredDlls do
		yield (dll,Async.StartAsTask(runProcessAsync (vsInstrPath,"/coverage " + dll)))
	}
	
for t in tasks do
	t |> Dump
//dlls |> Dump

//  start vsperfcmd in coverage mode ************************************************************************
let vsPerf = Async.StartAsTask(runProcessAsync (vsPerfCmdPath, "/start:coverage /output:"+coverageFileName))

//launch app
// launch iis express via command line
// http://www.iis.net/learn/extensions/using-iis-express/running-iis-express-from-the-command-line
let iis = Async.StartAsTask(runProcessAsync ( "c:\Program Files (x86)\IIS Express\iisexpress.exe", """/site:"CVS.Manage.Web" """))
// run manual or automated tests

iis.Result |> Dump
// vsperfcmd /shutdown
let perfOutput =Async.StartAsTask( runProcessAsync (vsPerfCmdPath, "/shutdown")).Result

perfOutput |> Dump

//check for coverage file

match System.IO.File.Exists(coverageFileName) with
	| true -> 
		let fullPath = System.IO.Path.GetFullPath(coverageFileName)
		(LINQPad.Hyperlinq(fullPath),System.IO.FileInfo(coverageFileName)).Dump("Coverage file created at " + coverageFileName)
	| _ -> "Could not find coverage file" |> Dump