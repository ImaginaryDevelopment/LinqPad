<Query Kind="FSharpProgram">
  <NuGetReference>Suave</NuGetReference>
</Query>

// listen for http requests, respond

// client could listen for volume down and relay it to the server

open Suave
let address = defaultConfig.bindings |> Seq.head |> fun x -> x |> string // |> Dump |> ignore
let (startedOptions,server) = startWebServerAsync defaultConfig (Successful.OK "Hello World!")
startedOptions.Dump()
let cts = new System.Threading.CancellationTokenSource()
Async.Start(server, cts.Token)

startedOptions |> Async.RunSynchronously |> printfn "started: %A"
// open browser for server
Process.Start(address) |> fun x -> Util.OnDemand(sprintf "Browser process %i" x.Id, fun () -> x).Dump()

Util.ReadLine() |> ignore

cts.Cancel()


