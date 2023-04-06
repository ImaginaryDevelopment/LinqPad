<Query Kind="FSharpProgram">
  <NuGetReference>Giraffe</NuGetReference>
  <IncludeAspNet>true</IncludeAspNet>
</Query>

// appears to be working, not fully tested, needs work on landing page as a menu perhaps?
// does not work: clipboard access after startup, printfn doesn't work in kestrel context either

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

let warnOnHttpsFailure = false

module Option =
    let ofValueString =
        function
        | null | "" -> None
        | x when String.IsNullOrWhiteSpace x -> None
        | x -> Some x
type Prop<'t>(initialValue, getSideEffect, beforeSetSideEffect, afterSetSideEffect) =
    let mutable value = initialValue
    member _.PropValue
        with get() = getSideEffect(); value
        and set v =
            beforeSetSideEffect (value,v)
            let oldValue = value
            value <- v
            afterSetSideEffect(oldValue,v)
    
let getQueryValue (name:string) (c:Microsoft.AspNetCore.Http.HttpContext) = c.Request.Query.[name]

let useContext f1 f2 = (fun x y -> f2 (f1 y) x y )

let webApp =
    // this doesn't appear to work at runtime, only startup
    let getClip() : string option = System.Windows.Forms.Clipboard.GetText() |> Option.ofValueString
    // this doesn't seem to work at all in the kestrel context
    let setClip (text:string) = System.Windows.Forms.Clipboard.SetText text
    
    // adapt to the clipboard read not working from this process, allow pushing to it
    let clip = Prop(getClip(), ignore, ignore, (fun (_,vOpt) -> vOpt |> Option.iter setClip ))
    let setClip value =
        value
        |> Option.bind Option.ofValueString
        |> function
            | None ->
                "Error: No text found"
            | Some v ->
                clip.PropValue <- Option.ofValueString v
                printfn "Storing text"
                printfn "%s" v
                sprintf "Stored text '%s'" v
    let sendText f x y =
        text (f()) x y
    let defaultText = "null"
    choose [
        route "/hello" >=> text "world"
        // does not work
        //route "/getclip" >=> (fun x y -> text(System.Windows.Forms.Clipboard.GetText()) x y)
        route "/setclip" >=> (useContext (getQueryValue "value" >> string >> Option.ofValueString >> setClip) text)
        route "/getclip" >=> sendText (fun () -> clip.PropValue |> Option.defaultValue defaultText)
        route "/getclipjson" >=> (fun x -> json (getClip()) x)
        
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
            
            x.Listen(ip,port, (fun listenOptions -> 
                try
                    listenOptions.Protocols <- Microsoft.AspNetCore.Server.Kestrel.Core.HttpProtocols.Http1AndHttp2
                    listenOptions.UseHttps()
                    |> ignore
                with ex ->
                    if warnOnHttpsFailure then
                        printfn "Failed to setup https, this may be fine for your use case"
                        ex.Dump()    
                ()
                )
            )
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