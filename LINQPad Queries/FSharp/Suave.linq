<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>Suave</NuGetReference>
</Query>

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
                [ GET >=> path "/public" >=> OK (sprintf "Hello anonymous we are in %s" Environment.CurrentDirectory)
                  // access to handlers after this one will require authentication
                  Authentication.authenticateBasic ((=) ("foo", "bar")) <|
                  //GET >=> path "/protected" >=> request (fun x -> OK ("Hello " + (x.userState.["userName"]|> string))) 
                    choose [
                        GET >=> path "/whereami" >=> OK (sprintf "Hello authenticated person you are currently at %s or possibly %s" Environment.CurrentDirectory (Assembly.GetExecutingAssembly().CodeBase.ToString()))
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
                    member x.Log level f = 
                        let logLine = f()
                        logLine.Dump()
                        |> ignore
                        
            }
        
        startWebServer {defaultConfig with logger = logger} (Successful.OK "Hello World!")
            
let startServer serverType = 
    match serverType with
    | HelloWorld -> ServerSamples.helloWorld()
    | Routing -> ServerSamples.routing ()
    | BasicAuth -> ServerSamples.basicAuth()
    | Logging -> ServerSamples.logging()
    // TODO: Async section
//        | Async -> 
//            let sleep milliseconds message: WebPart =
//                fun (x : HttpContext) ->
//                async {
//                  do! Async.Sleep milliseconds
//                  return! OK message x
//                }
            
startServer Logging