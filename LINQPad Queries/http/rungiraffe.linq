<Query Kind="FSharpProgram">
  <NuGetReference>Giraffe</NuGetReference>
  <IncludeAspNet>true</IncludeAspNet>
</Query>

// basing on micro kestrel example: https://gist.github.com/poke/665261ea1e4bcf233f9712ed5cb3c4f1
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

let webApp =
    choose [
        route "/hello" >=> text "world"
        // does not work
        route "/getclip" >=> (fun x y -> text(System.Windows.Forms.Clipboard.GetText()) x y)
        route "/clip" >=> (fun x y -> text (string y.Request.Query.["value"]) x y)
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