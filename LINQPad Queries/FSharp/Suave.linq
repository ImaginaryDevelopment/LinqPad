<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>Suave</NuGetReference>
</Query>

// imaginarydevelopment.blogspot.com
// @maslowjax

open Suave
open Suave.Files
open Suave.Filters
open Suave.Successful
open Suave.Operators
open Suave.Authentication

//needs: serve files, serve pages
type Tutorial = 
    |HelloWorld
    |Routing
    |Async //TODO: implementation
    |BasicAuth
    |Logging
module ServerSamples = 
    let helloWorld () = 
        startWebServer defaultConfig (Successful.OK "Hello World!")
        
    let routing () = 
        choose 
            [
                GET >=> choose
                    [   
                        path "/" >=> OK "Default GET"
                        path "/hello" >=> OK "Hello GET"
                        path "/goodbye" >=> OK "Good bye GET"
                    ]
                POST >=> choose
                    [
                        path "/hello" >=> OK "Hello POST"
                        path "/goodbye" >=> OK "Good bye POST"
                    ]
            ]
        |> startWebServer defaultConfig
    
    let basicAuth () = 
        let requiresAuthentication _ =
            choose
                [ GET >=> path "/public" >=> OK "Default GET"
                  // access to handlers after this one will require authentication
                  Authentication.authenticateBasic ((=) ("foo", "bar")) <|
                    choose [
                        GET >=> path "/whereami" >=> OK (sprintf "Hello authenticated person ")
                        GET >=> path "/" >=> dirHome
                        GET >=> browseHome //serves file if exists                       
                    ]
                ]
        requiresAuthentication()
        |> startWebServer defaultConfig
    let logging () = 
        let logger = 
            {
                new Suave.Logging.Logger with
                    member __.name = [| "My custom logger" |]
                    member __.logWithAck logLevel f =
                        let x = f logLevel
                        Async.result ()
                    member __.log level f = 
                        let logLine = f level
                        logLine.Dump()
                        |> ignore
                        
            }
        
        startWebServer {defaultConfig with logger = logger} (Successful.OK "Hello World!")
    let async() = 
        let sleep milliseconds message : WebPart = 
            fun (x:HttpContext) ->
                async {
                    do! Async.Sleep milliseconds
                    return! OK message x
                }
        startWebServer defaultConfig (sleep 50 "Hello World Async!")
        
let startServer serverType = 
    match serverType with
    | HelloWorld -> ServerSamples.helloWorld()
    | Routing -> ServerSamples.routing ()
    | BasicAuth -> ServerSamples.basicAuth()
    | Logging -> ServerSamples.logging()
    | Async -> ServerSamples.async ()
            
LINQPad.Hyperlinq("http://localhost:8083").Dump()
LINQPad.Hyperlinq("http://localhost:8083/public").Dump()
startServer Async