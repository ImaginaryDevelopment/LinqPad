<Query Kind="FSharpProgram">
  <NuGetReference>BrokenDiscord</NuGetReference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <Namespace>BrokenDiscord.Client</Namespace>
  <Namespace>BrokenDiscord.Events</Namespace>
  <Namespace>BrokenDiscord.Types</Namespace>
  <Namespace>Hopac</Namespace>
</Query>

module Option =
    let getOrDefault y = function | Some x -> x | None -> y
module Discordantly =
    let dumpAuthorizeForChannelLink () =
        Util.GetPassword "Client ID"
        |> sprintf "https://discordapp.com/oauth2/authorize?&client_id=%s&scope=bot&permissions=0"
        |> fun x -> LINQPad.Hyperlinq(x,"Add Bot to Server link")
        |> LINQPad.Extensions.Dump
        |> ignore
    let pong (client:Client) (m : Message) =
        job {
            if m.content = "!ping" then
                return! client.CreateMessage m.channelId <| MessageCreate.T.New "pong!"
                        |> Job.startIgnore
            else return ()
        } |> start
    
    let handleEvents client = function
        | MessageCreate m -> (pong client m)
        | _ -> ()
open Discordantly

module Reflection =
    type System.Reflection.Assembly with
        member x.Name = 
            x.FullName 
            |> Option.ofObj
            |> Option.map (fun n -> n.Split(',').[0])
            |> Option.getOrDefault x.FullName
        member x.Version =
            try
                x.GetName().Version.ToString()
            with ex -> ex.Dump("Bad version attempt for " + x.Name); "Unknown"
    type LocationStage = 
        | AN of AssemblyName
        | A of Assembly
        with member x.ToDump() =
                match x with
                | AN an ->
                    sprintf "%s - %A" an.Name an.Version
                | A a -> sprintf "%s - %A" a.Name a.Version
    
    let getRefAssemblies () = 
        let needsTypeReferenceToLoad = 
            [
                typeof<Newtonsoft.Json.Formatting>.Assembly
                typeof<Hopac.EmbeddedJob<_>>.Assembly
            ]
            |> Seq.map (fun a -> a.Name, a)
            |> dict
        
        Assembly.GetExecutingAssembly().GetReferencedAssemblies()
        |> Seq.sortBy(fun ra -> ra.Version)
        |> List.ofSeq
        |> List.rev
        |> Seq.map (fun a -> 
            a.Name, 
                    if needsTypeReferenceToLoad.ContainsKey a.Name then
                        A needsTypeReferenceToLoad.[a.Name]
                    else AN a
        )
open Reflection

dumpAuthorizeForChannelLink()
let main _argv =
    use client = new Client(Util.GetPassword "BotToken")
    client.Events
    |> Event.add (handleEvents client)
    getRefAssemblies().Dump()
    printfn "Listening for pings..."
    client.start()
    printfn "Press enter to quit"
    stdin.ReadLine () |> ignore
    0
main Array.empty
|> ignore<int>