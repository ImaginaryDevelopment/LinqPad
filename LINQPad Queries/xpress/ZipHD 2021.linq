<Query Kind="FSharpProgram" />

// zip/deploy/release/drop/publish (not clickonce) HD client wpf
// also server
type PublishType =
    |Client
    |Server
    |Both
type Configuration = Debug | Release
let publishType = Both
let configuration = Release
let iteration = 4
type BuildCommand = {Cmd:string;Args:string;WorkingDirectory:string}
type App = { SourceDir:string;DropPath:string;ZipTitling:string;BuildCommand:BuildCommand option}
let apps =
    [
        let clientDir = @"C:\projects\HealthDesigns\Client\HD.Client.WPF\" 
        let client = {
            SourceDir = sprintf @"bin\%A"configuration 
            DropPath = @"C:\projects\HealthDesigns\Drop\Client"
            ZipTitling =  sprintf "HD.Client.WPF.%A_%s_%02i.zip" configuration (DateTime.Now.ToString("yyyy.MM.dd")) iteration
            BuildCommand = Some {Cmd="msbuild";Args="-c Release";WorkingDirectory = 
        }
        let serverDir = @"C:\projects\HealthDesigns\safe\src\Server"
        let serverout = Path.Combine(serverDir, sprintf @"bin\%A\netcoreapp3.0" configuration)
        let server =  { // needs safe\src\Server>dotnet build -c Release
            SourceDir = serverout 
            DropPath = @"C:\projects\HealthDesigns\Drop\Server"
            ZipTitling = sprintf "HD.Server.%A_%s_%02i.zip" configuration (DateTime.Now.ToString("yyyy.MM.dd")) iteration
            BuildCommand = Some {Cmd="dotnet";Args="build -c Release";WorkingDirectory=serverDir}
        }
        match publishType with
        | Server -> yield server
        | Client -> yield client
        | Both -> yield! [server;client]
    ]
    
let run {SourceDir=src;DropPath=drp;ZipTitling=z;BuildCommand=bc} =

//let targetName = sprintf "HD.Client.WPF.%s_%s_%02i.zip" publishTypeName (DateTime.Now.ToString("yyyy.MM.dd")) iteration
    let targetFullPath = Path.Combine(drp, z)
    // TODO: do it boy
    let moveDataOutAndBack (): IDisposable = raise <| NotImplementedException("We need to not zip up the data folder")

    let slightlyCleanOutput () =
        let oldPublish = Path.Combine(src, "app.publish")
        if Directory.Exists oldPublish then
            Directory.Delete(oldPublish,recursive=true)
        if Directory.Exists src then
            try
                Directory.GetFiles(src,"*.zip")
                |> Seq.iter(fun d -> Directory.Move(d,Path.Combine(drp,Path.GetFileName(d))))
            with ex ->
                eprintfn "Failing to get zip files from %s" src
                reraise()

    slightlyCleanOutput()
    match bc with
    | None -> ()
    | Some bc -> 
        let psi = ProcessStartInfo(bc.Cmd, bc.Args)
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.WorkingDirectory <- bc.WorkingDirectory
        let ps = Process.Start psi
        ps.WaitForExit()
        ps.StandardOutput.ReadToEnd().Dump("std")
        ps.StandardError.ReadToEnd().Dump("Error stream")
        if ps.ExitCode <> 0 then failwithf "Build failed: %i" ps.ExitCode
    printfn "Reading '%s'" src
    if Directory.EnumerateFiles src |> Seq.length < 3 then failwithf "more files expected in '%s" src
    System.IO.Compression.ZipFile.CreateFromDirectory(src, targetFullPath)
    let targetDir = Path.GetDirectoryName targetFullPath
    let f = Action(fun () -> Process.Start (sprintf "explorer -e \"%s\"" targetDir) |> ignore)
    (Hyperlinq (f,text= targetDir)).Dump()

apps
|> Seq.iter run 