<Query Kind="FSharpProgram" />

// http://stackoverflow.com/questions/4958776/batch-running-of-vs-code-coverage-tools
// 2010 version: http://generichuman4897.blogspot.com/2010/12/enabling-code-coverage-in-vs2010.html
// 2010 version2: http://scissortools.wordpress.com/2011/12/03/code-coverage-test-in-visual-studio-2010-things-to-be-aware-of/
let vsInstrPath = """C:\Program Files (x86)\Microsoft Visual Studio 12.0\Team Tools\Performance Tools\vsinstr.exe"""
let vsPerfCmdPath = """C:\Program Files (x86)\Microsoft Visual Studio 12.0\Team Tools\Performance Tools\VSPerfCmd.exe"""
//helpers 
type ProcessResult = { exitCode : int; stdout : string; stderr : string }

let runProcess (fileName:string,args:string) =
	let psi = new System.Diagnostics.ProcessStartInfo(fileName,args,UseShellExecute=false, RedirectStandardOutput=true, RedirectStandardError=true, CreateNoWindow=true)
	// psi |> Dump
	let p = System.Diagnostics.Process.Start(psi)
	p.WaitForExit()
	(p.ExitCode,p.StandardOutput.ReadToEnd(),p.StandardError.ReadToEnd())
//let runProcessAsync 	
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

//instrument  ******************************************************************************************
// VsInstr http://msdn.microsoft.com/en-us/library/ms182402.aspx
let dlls = System.IO.Directory.GetFiles(binDir, "*.dll")
let filteredDlls = dlls |> Seq.filter (fun x -> 
	let fileName=System.IO.Path.GetFileName(x)
	fileName.Contains("CVS") || fileName.Contains("Oceanside"))
//filteredDlls |> Dump
for dll in dlls do
	(dll,runProcess (vsinstrPath,"/coverage " + dll)) |> Dump
//dlls |> Dump

//  start vsperfcmd in coverage mode ************************************************************************

//runProcess(vsPerfCmdPath,"/start:coverage /output:run.coverage")
//launch app
// launch iis express via command line
// http://www.iis.net/learn/extensions/using-iis-express/running-iis-express-from-the-command-line

// run manual or automated tests

// vsperfcmd /shutdown