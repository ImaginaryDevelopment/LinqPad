<Query Kind="FSharpProgram" />

open System

type ProcessResult = { exitCode : int; stdout : string; stderr : string }

let guard f (e:IEvent<'Del, 'Args>) = // http://stackoverflow.com/a/2658530/57883
    let e = Event.map id e
    { new IEvent<'Args> with 
        member x.AddHandler(d) = e.AddHandler(d); f() //must call f here!
        member x.RemoveHandler(d) = e.RemoveHandler(d)
        member x.Subscribe(observer) = 
          let rm = e.Subscribe(observer) in f(); rm }
		  
//http://stackoverflow.com/questions/3065409/starting-a-process-synchronously-and-streaming-the-output
let runProcessAsync (fileName:string,args:string) = async {
	let psi = new System.Diagnostics.ProcessStartInfo(fileName,args,UseShellExecute=false, RedirectStandardOutput=true, RedirectStandardError=true) //, CreateNoWindow=true)
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
let np =Async.StartAsTask (runProcessAsync ("notepad.exe","") )
let c = Async.StartAsTask (runProcessAsync ("cmd","/c dir"))

c.Result |> Dump
np.Result |> Dump
