<Query Kind="FSharpProgram">
  <NuGetReference Version="8.0.1">Newtonsoft.Json</NuGetReference>
  <NuGetReference>SlackTypeProvider</NuGetReference>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

open SlackProvider
let token = lazy (Util.GetPassword("SlackSmartF#ers-Random")) 
let setupTokenFileForTP () = 
    let token = token.Value
    let ud = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData)
    if not <| Directory.Exists ud then
        failwithf "no local appdata folder found"
    let parent = Path.Combine(ud, "slackApi")
    if not <| Directory.Exists parent then
        Directory.CreateDirectory(parent) |> ignore
    let path = Path.Combine(parent,"token.txt")
    path.Dump()
    File.WriteAllText(path, token)

Newtonsoft.Json.JsonConvert.SerializeObject( new obj()) |> ignore<string>
//setupTokenFileForTP()
// this is primarily to use reflection to hook locate the necessary assemblies to push this over to a .fsx file since it doesn't seem to work here.
let useManualClient () = 
    token.Value
    |> SlackProvider.Models.SlackClient
    |> fun sc -> sc.SendMessage (fun x -> {x with Text = "Hello slack world"})
    |> Dump
    |> ignore
module ReflectYourselfFool = 
    type System.Reflection.Assembly with
        member x.Name = 
            x.FullName 
            |> Option.ofObj
            |> Option.map (fun n -> n.Split(',').[0])
            |> Option.getOrDefault x.FullName
            
    // tp could not load Newtonsoft, force load it perhaps
    let listenToResolveAttempts () = 
        let onResolveEvent = new ResolveEventHandler( fun sender evArgs ->
            //let requestedAssembly = AssemblyName(evArgs.Name)
            printfn "Found resolve event"
            try
                let requestingAssembly = if not <| isNull evArgs.RequestingAssembly then evArgs.RequestingAssembly.CodeBase else "null"
                printfn "requested assembly:%s by %s" evArgs.Name requestingAssembly
            with ex -> ex.Dump()
            null
            )
        //AppDomain.CurrentDomain.TypeResolve.Add (fun e -> printfn "Attempting to resolve %s" e.Name)
        AppDomain.CurrentDomain.add_AssemblyResolve onResolveEvent
        
    type LocationStage = 
        | AN of AssemblyName
        | A of Assembly
        

    let getRefAssemblies () = 
        let needsTypeReferenceToLoad = 
            [
                typeof<Newtonsoft.Json.Formatting>.Assembly
                typeof<SlackProvider.Models.SlackClient>.Assembly
                typeof<BReusable.Railways.Railway<string,string>>.Assembly
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

    let getLocation = 
        function
            | AN an ->
                try
                    if isNull an.CodeBase then Assembly.ReflectionOnlyLoad(an.FullName).Location else an.CodeBase 
                with ex -> ex.Dump();null
            | A a -> a.Location
try
    useManualClient()
with ex ->
    ReflectYourselfFool.getRefAssemblies()
    |> Seq.map (fun (n, a) -> n , ReflectYourselfFool.getLocation a)
    |> Dump
    |> Seq.map (snd >> fun l -> sprintf "#r @\"%s\"" l)
    |> delimit "\r\n"
    |> Dump |> ignore
    reraise ()
    
//type TSlack = SlackTypeProvider<token= @"C:\Users\Brandon\AppData\Local\slackApi\token.txt">
//
//let slack = TSlack()
//
//slack.Channels.Random.Send("Test, Hello slack bot")