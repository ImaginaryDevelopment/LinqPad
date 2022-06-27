<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.IO.Compression.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.IO.Compression.FileSystem.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.IO.Compression.ZipFile.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <Namespace>System.IO.Compression</Namespace>
</Query>

// publish/release HD stuffs
// 2019.10.07 - does not work, hacked at it, and used fake build -t package instead from terminal
type PathType =
    |Dir of string
    |File of string
let rec recIt path =
    seq{ 
        let dirs = System.IO.Directory.GetDirectories path
        let files = System.IO.Directory.GetFiles path
        yield! files |> Seq.map PathType.File
        for d in dirs do
            yield! recIt d
            yield Dir d
    }
let rmRF path =
    recIt path
    |> Seq.iter(
        function |PathType.File f -> System.IO.File.Delete f | PathType.Dir d -> System.IO.Directory.Delete d
    )
let findNewest path =
    recIt path
    |> Seq.choose(function | PathType.File f -> Some f |PathType.Dir _ -> None)
    |> Seq.map(fun f -> System.IO.FileInfo f |> fun fi -> f,fi.LastWriteTime)
    |> Seq.maxBy(fun (_,w) -> w)
let fakeBuild p =
    let oldd = Environment.CurrentDirectory
    Environment.CurrentDirectory <- p
    Util.Cmd("fake","build -t Package")
    |> Dump
    |> ignore
    Environment.CurrentDirectory <- oldd
let publish iteration relSrc publishTypeName = 
    let buildPath = @"C:\projects\HealthDesigns\safe"
    let sourcePath = sprintf @"%s\%s" buildPath relSrc
    printfn "Using %s" sourcePath
    if not <| System.IO.Directory.Exists sourcePath then
        failwithf "Source did not exist at %s" sourcePath
    let targetName = sprintf "HD%s%s.%s.zip" publishTypeName (DateTime.Now.ToString("yyyyMMdd")) iteration
    let outputPath = sprintf @"C:\projects\HealthDesigns\Drop\Server\%s" targetName
    fakeBuild sourcePath
    let newestFn,dt = findNewest sourcePath
    if not (Util.ReadLine<bool>(sprintf "Publish from %s last updated on %A(%s)?" sourcePath dt newestFn)) then
        raise (Exception "aborted")
//    System.IO.Compression.ZipFile.CreateFromDirectory(sourcePath, outputPath)
//    try
//        if Util.ReadLine<bool>(sprintf "clean release binaries from %s?" sourcePath) then
//        rmRF sourcePath
//    with ex ->
//        printfn "Delete incomplete %s" ex.Message
//        ()
//        
    outputPath
let publishServer iteration = 
//    publish iteration @"safe\src\Server\bin\release\netcoreapp2.2\publish" "Service"
    publish iteration @"deploy" "Service"
    
//let publishClient iteration = 
//    publish iteration @"Client\HD.Client.WPF\bin\Release" "Client"
// client can publish directory if server machine is present    
//publishClient "01" |> ignore<unit>
printfn "use VS Release -> Publish for client"
let target = publishServer "01"
printfn "published to %s on %A" target  DateTime.Now