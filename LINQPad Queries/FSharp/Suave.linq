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
    |Async
    |BasicAuth
    |Logging
module ServerSamples = 
    let helloWorld () = 
        startWebServer defaultConfig (Successful.OK "Hello World!")
        
    let routing () = 
        let (|ParseInt|_|) =
            function
            | null | "" -> None
            | x -> 
                match Int32.TryParse x with
                | true, i -> Some i
                | _ -> None
            
            
        // this only returns a string but hopefully it helps imply how more complicated items could be composed
        let queryParamOrFail name (ctx:HttpContext) =
            match ctx.request.queryParam name with
            | Choice1Of2 value -> 
                Choice1Of2 value
            | Choice2Of2 msg ->
                RequestErrors.BAD_REQUEST msg
                |> Choice2Of2
        let queryIntOrFail name =
            queryParamOrFail name
            >> Choice.bind(
                (|ParseInt|_|)
                >> function
                    | Some i -> Choice1Of2 i
                    | None -> RequestErrors.BAD_REQUEST (sprintf "query param %s was not a number" name) |> Choice2Of2
            )
        let clientQueryPart:WebPart =
            path "/clientQuery" >=>
            (fun ctx ->
                queryIntOrFail "companyId" ctx
                |> function
                    | Choice1Of2 v -> sprintf "CompanyId %i" v |> OK
                    | Choice2Of2 requestErrorWebPart -> requestErrorWebPart 
                |> fun wp -> wp ctx
            )
        let fullQueryPart:WebPart =
            path "/query" >=>
            (fun ctx ->
                match queryIntOrFail "companyId" ctx, queryIntOrFail "clientId" ctx, queryIntOrFail "customerId" ctx with
                | Choice2Of2 reqErr,_,_ -> reqErr
                | _,Choice2Of2 reqErr,_ -> reqErr
                | _,_,Choice2Of2 reqErr -> reqErr
                | Choice1Of2 compId, Choice1Of2 clientId, Choice1Of2 customerId ->
                    sprintf "CompanyId %i, ClientId %i, CustomerId %i" compId clientId customerId
                    |> OK
                |> fun wp -> wp ctx
            )
        // https://stackoverflow.com/a/36549318/57883
        let requestVersionPart name:WebPart =
            request (fun r ->
                let qValue=
                    match r.queryParam name with
                    |Choice1Of2 x -> x
                    |Choice2Of2 _ -> "defaultValue"
                OK (sprintf "The value we'll use for %s is %s" name qValue)
            )
        let troubleShootExtensionPart extensionToCheck :WebPart =
            fun ctx ->
                match extensionToCheck with
                | null | "" -> ServerErrors.INTERNAL_ERROR "Extension Error not supplied, part is not set up correctly"
                | x when not <| x.StartsWith "." -> ServerErrors.INTERNAL_ERROR "Extensions start with a '.', part is not set up correctly"
                | _ ->
                    let mtm = ctx.runtime.mimeTypesMap
                    match mtm extensionToCheck with
                    | None ->
                        sprintf "%s is not supported by the mime types map, compose your mime type with the `defaultMimeTypesMap`" extensionToCheck
                        |> RequestErrors.FORBIDDEN
                    | Some x ->
                        sprintf "%s is supported and uses '%s', compression on? : %A" extensionToCheck x.name x.compression
                        |> OK
                |> fun wp -> wp ctx
        choose 
            [
                GET >=> choose
                    [   
                        path "/" >=> OK "Default GET"
                        path "/hello" >=> OK "Hello GET"
                        pathScan "/whatnumber/%i" ((sprintf "Your number is %i") >> OK)
                        pathScan "/client/%i/customer/%i" (fun (clientId,customerId) -> sprintf "Client %i, Customer %i" clientId customerId |> OK)
                        pathScan "/client/%i/customerQuery" (fun clientId ctx -> 
                            match queryParamOrFail "customerId" ctx with
                            | Choice1Of2 (ParseInt customerId) ->
                                sprintf "Client %i, Customer %i" clientId customerId
                                |> fun msg ->  OK msg ctx
                            | Choice1Of2 _ -> RequestErrors.BAD_REQUEST "query param customerId was not a number" ctx
                            | Choice2Of2 wp -> wp ctx
                        )
                        clientQueryPart
                        fullQueryPart
                        path "/req" >=> requestVersionPart "customerId"
                        path "/goodbye" >=> OK "Good bye GET"
                        #if DEBUG
                        pathScan "/checkExtension/%s" (fun name -> troubleShootExtensionPart name)
                        // catch all
                        (fun ctx -> sprintf "404, also homeFolder resolves to %s" (Path.GetFullPath ".") |> RequestErrors.NOT_FOUND |> fun wp -> wp ctx)
                        #endif
                        
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
                        let _x = f logLevel
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
//startServer Async
startServer Routing