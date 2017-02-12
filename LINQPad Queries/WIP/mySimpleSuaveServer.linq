<Query Kind="FSharpProgram">
  <Reference Relative="..\..\..\..\FsInteractive\MacroRunner\MacroRunner\bin\Debug\MacroRunner.exe">C:\projects\FsInteractive\MacroRunner\MacroRunner\bin\Debug\MacroRunner.exe</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.ComponentModel.Composition.dll</Reference>
  <NuGetReference>Suave</NuGetReference>
  <Namespace>Suave</Namespace>
  <Namespace>Suave.Filters</Namespace>
  <Namespace>Suave.Operators</Namespace>
  <Namespace>Suave.Successful</Namespace>
  <Namespace>System.ComponentModel.Composition</Namespace>
  <Namespace>System.ComponentModel.Composition.Hosting</Namespace>
</Query>

// listen for http requests, respond
let dumpt t x = x.Dump(description = t); x
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
// following https://blogs.msdn.microsoft.com/jomo_fisher/2010/03/09/neat-samples-extend-your-f-program-with-mef/
module Mef =
    let copyFile fp src =
        if not <| File.Exists fp || FileInfo(src).LastWriteTimeUtc > FileInfo(fp).LastWriteTimeUtc then
            File.Copy(src,fp, true) 
    let mefFolder =
        let tempLocation = System.Reflection.Assembly.GetExecutingAssembly().Location |> dumpt "location" |> Path.GetDirectoryName
        tempLocation.Dump()
        Environment.CurrentDirectory <- tempLocation
        let targetDll = @"C:\projects\FsInteractive\MacroRunner\BMeffers\bin\Debug\BMeffers.dll"
        let mefFolder = Path.Combine(tempLocation,"extensions")
        Directory.CreateDirectory(mefFolder) |> ignore
        do
            let targetDllPath = Path.Combine(mefFolder, Path.GetFileName targetDll) 
            copyFile targetDllPath targetDll
            let targetPdbPath = Path.Combine(mefFolder, Path.GetFileNameWithoutExtension targetDll |> flip (+) ".pdb")
            copyFile targetPdbPath (Path.Combine(Path.GetDirectoryName(targetDll), Path.GetFileNameWithoutExtension(targetDll) + ".pdb"))
        mefFolder
    type Composer() =
        [<ImportMany(typeof<MacroRunner.Schema.ISideEffect>)>]
        let effects: seq<MacroRunner.Schema.ISideEffect> = null
        member x.Effects = effects
    let jar = Composer()
    
    let catalog = new AggregateCatalog()
    let directoryCatalog = new DirectoryCatalog(mefFolder, "*.dll")
    let container = new CompositionContainer(catalog)
    catalog.Catalogs.Add(directoryCatalog)
    container.ComposeParts(jar)
    jar.Effects |> Seq.map (fun e -> e.Name) |> List.ofSeq |> dumpt "effects!" |> ignore
let landingPage = 
    Mef.jar.Effects |> Seq.map(fun e -> sprintf "<a title=\"%s\" href=\"effects/%s\">%s</a>" e.Description e.Name e.Name) 
    |> delimit "\r\n\t"
    |> sprintf """<html>
<head></head>
<body>
<h2>You just got served</h2>
<a href="/helloworld">Hello World</a>
%s
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
                    pathStarts "/effects/" >=> request (fun req-> 
                        req.url.PathAndQuery 
                        |> after "/effects/" 
                        |> fun name -> 
                            name |> dumpt "looking for effect" |> ignore
                            Mef.jar.Effects |> Seq.find(fun e -> e.Name= name) |> fun e -> e.Execution()
                            OK (sprintf "Executed %s" name)
                        )
                    path "/sideeffect" >=> request (sideeffect testSideEffect (fun _req -> OK "Side Effected"))
                    //path "/sideeffect2" >=> request (fun x -> testSideEffect(); x) >=> OK "Side effected"
                    path "/sideeffect2"  >=> request (fun x -> testSideEffect(); OK "Side Effected")
                    path "/sideeffect3" >=> (lift testSideEffect >> OK "Side Effected 3")
                    path "/helloworld" 
                        >=> request volumeDown //|> sideeffect (fun x -> volumeDown()) |> OK "Volume turning down"

                    ] ]
                    
let run () =
    use catalog = Mef.catalog
    use directoryCat = Mef.directoryCatalog
    use container = Mef.container
    serveIt cts.Token app
    |> fun address -> Process.Start(address) |> fun x -> Util.OnDemand(sprintf "Browser process %i" x.Id, fun () -> x).Dump()

    Util.ReadLine() |> ignore

    cts.Cancel()
run()