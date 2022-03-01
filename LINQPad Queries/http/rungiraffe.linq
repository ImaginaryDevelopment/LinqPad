<Query Kind="FSharpProgram">
  <NuGetReference>Giraffe</NuGetReference>
  <IncludeAspNet>true</IncludeAspNet>
</Query>

// appears to be working, not fully tested, needs work on landing page as a menu perhaps?
open System
open System.Net

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
//open Microsoft.AspNetCore.Hosting.Internal
open Microsoft.AspNetCore.Hosting.Server
//open Microsoft.AspNetCore.Http
//open Microsoft.AspNetCore.Http.Features

open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging


open Giraffe

module Option =
    let ofValueString =
        function
        | null | "" -> None
        | x when String.IsNullOrWhiteSpace x -> None
        | x -> Some x
        
let getQueryValue (name:string) (c:Microsoft.AspNetCore.Http.HttpContext) = c.Request.Query.[name]

let useContext f1 f2 = (fun x y -> f2 (f1 y) x y )

let webApp =
    // adapt to the clipboard read not working from this process, allow pushing to it
    let mutable clipText = System.Windows.Forms.Clipboard.GetText() |> Option.ofValueString
    let setClip value =
        value
        |> Option.ofValueString
        |> function
            | None ->
                clipText <- None
                "Cleared text"
            | Some v ->
                clipText <- Some v
                sprintf "Stored text '%s'" v
    let sendText f x y =
        text (f()) x y
    let defaultText = "null"
    choose [
        route "/hello" >=> text "world"
        // does not work
        //route "/getclip" >=> (fun x y -> text(System.Windows.Forms.Clipboard.GetText()) x y)
        route "/setclip" >=> (useContext (getQueryValue "value" >> string >> setClip) text)
        route "/getclip" >=> sendText (fun () -> clipText |> Option.defaultValue defaultText)
        
        // not using get/put
        route "/clip" >=> (fun x y ->
            y.Request.Query.["value"]
            |> string
            |> Option.ofValueString
            |> Option.map setClip
            |> function
                | None -> // assume get
                    text (clipText |> Option.defaultValue defaultText) x y
                | Some v -> text v x y
        )
        route "/" >=> text "hello there"
        route "" >=> text "hello there"
        
    ]
    
let configureApp handlerOpt (app:IApplicationBuilder) =
    app.UseGiraffe webApp
    handlerOpt
    |> Option.iter(app.UseGiraffeErrorHandler>>ignore)
let configureServices' (x:IServiceCollection) =
    x.AddGiraffe() |> ignore
let runWhbAdapter (ip:System.Net.IPAddress) port (whb:IWebHostBuilder) =
        whb.ConfigureKestrel(fun x ->
            x.Listen(ip,port)
        )
        |> ignore
        whb.Configure (configureApp None) |> ignore
        whb.ConfigureServices configureServices' |> ignore
let ipAddress = Util.ReadLine("IPAddress?",Util.GetPassword("IPAddress")) |> IPAddress.Parse
let port = Util.ReadLine("Port?", Util.GetPassword("Port") |> Option.ofValueString |> Option.map int |> Option.defaultValue 8008)
let run () =
    Host.CreateDefaultBuilder().ConfigureWebHostDefaults(Action<_> (runWhbAdapter ipAddress port)).Build().Run()
    
run()
()