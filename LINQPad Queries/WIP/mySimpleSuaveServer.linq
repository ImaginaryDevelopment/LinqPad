<Query Kind="FSharpProgram">
  <NuGetReference>Suave</NuGetReference>
  <Namespace>Suave</Namespace>
  <Namespace>Suave.Filters</Namespace>
  <Namespace>Suave.Operators</Namespace>
  <Namespace>Suave.Successful</Namespace>
</Query>

// listen for http requests, respond

// client could listen for volume down and relay it to the server
// http://stackoverflow.com/questions/13139181/how-to-programmatically-set-the-system-volume
let serveIt token app =
    let address = defaultConfig.bindings |> Seq.head |> fun x -> x |> string // |> Dump |> ignore
    let (startedOptions,server) = startWebServerAsync defaultConfig app // (Successful.OK "Hello World!")
    startedOptions.Dump()
    
    Async.Start(server, token)
    
    startedOptions |> Async.RunSynchronously |> printfn "started: %A"
    // open browser for server
    address
    
let cts = new System.Threading.CancellationTokenSource()    
let testSideEffect () =
    printfn "running test side effect"
    
let sideeffect f fResult x= 
    f()
    fResult x
//let fPart f fResult: WebPart =
//    fun (x:HttpContext) ->
//        async{
//            f()
//            return fResult
//        }
let landingPage = """<html>
<head></head>
<body>
<h2>You just got served</h2>
<a href="volumedown">Volume Down</a>
</body>
</html>
"""
let lift f x =
    f()
    x
let volumeDown x = 
    printfn "volume down running"
    x.Dump()
    OK ""

let app = 
    choose 
        [ GET >=> choose
                [   path "/" >=> OK landingPage
                    path "/sideeffect" >=> request (sideeffect testSideEffect (fun _req -> OK "Side Effected"))
                    //path "/sideeffect2" >=> request (fun x -> testSideEffect(); x) >=> OK "Side effected"
                    path "/sideeffect2"  >=> request (fun x -> testSideEffect(); OK "Side Effected")
                    path "/sideeffect3" >=> (lift testSideEffect >> OK "Side Effected 3")
                    path "/volumedown" 
                        >=> request volumeDown //|> sideeffect (fun x -> volumeDown()) |> OK "Volume turning down"

                    ] ]
                    
serveIt cts.Token app
|> fun address -> Process.Start(address) |> fun x -> Util.OnDemand(sprintf "Browser process %i" x.Id, fun () -> x).Dump()

Util.ReadLine() |> ignore

cts.Cancel()