<Query Kind="FSharpProgram">
  <NuGetReference>Falco</NuGetReference>
  <IncludeAspNet>true</IncludeAspNet>
</Query>

// host file upload?
open Falco
open Falco.Routing
open Falco.HostBuilder

webHost [||] {
    endpoints [
        get "/" (Response.ofPlainText "Hello World")
    ]
}