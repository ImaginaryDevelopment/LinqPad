<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.IO.Compression.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.IO.Compression.FileSystem.dll</Reference>
  <Namespace>System.IO.Compression</Namespace>
  <DisableMyExtensions>true</DisableMyExtensions>
  <CopyLocal>true</CopyLocal>
</Query>

// iterate a build drop structure, getting a specific file's version info
// consider: adding the ability to query changeset number

let toCharArray(x:string) = x.ToCharArray()
let trimStart1 d (x:string) = x.TrimStart(d |> toCharArray)
let (@@) path subPath = 
    subPath 
    |> trimStart1 "\\"
    |> trimStart1 "/"
    |> fun x -> Path.Combine(path,subPath)
    
type ZipArchive with 
    member x.TryExtract fullName (target:string) =    
        x.Entries
        |> Seq.tryFind(fun ze -> 
            ze.FullName = fullName
        )
        |> Option.map(fun ze ->
            let lwt = ze.LastWriteTime
            use stream = ze.Open()
            use destination = File.Open(target,FileMode.CreateNew, FileAccess.Write, FileShare.None)
            stream.CopyTo destination
            printfn "Extracted %s to %s" fullName target
            // need the file handle closed before setting write time
            destination.Dispose()
            File.SetLastWriteTime(target,lwt.DateTime)
            target
        )
        
let targetish = Environment.ExpandEnvironmentVariables("%droproot%")
let targets = [ targetish; Path.GetDirectoryName targetish @@ "Pm_VsBuild_Debug_Deploy"]
printfn "Checking in %A" targets
let exeName = "PracticeManagement.exe"
let subPath = @"drop\PM\" + exeName
let (|ExeDrop|_|) d = 
    let fp = Path.Combine(d,subPath)
    if File.Exists fp then
        Some fp
    else 
        try
            let extractedP = Path.Combine(d,"drop",exeName) 
            if File.Exists extractedP then
                Some extractedP
            else 
                None
        with ex ->
            printfn "EXE Drop failing"
            reraise()
let (|ZipDrop|_|) d =
    let zipPath = Path.Combine(d,"drop")
    if Directory.Exists zipPath then
        Directory.GetFiles(zipPath,"*.zip") 
        |> Seq.tryHead
        |> function
            | Some zfp ->
                use zp = ZipFile.OpenRead(zfp)
                let targetEntry = "b/PM/" + exeName
                let targetOut = Path.Combine(zipPath,exeName)
                zp.TryExtract targetEntry targetOut
            | None -> None
    else None
let searchedButEmpty = ResizeArray()
let findDrops p = 
    let d =  Directory.EnumerateDirectories p |> List.ofSeq
    printfn "Searching %i directories under %s(%i unique)" d.Length p (d |> List.distinct |> List.length)
//    d.Dump()
    d
    |> Seq.filter(fun x -> Regex.IsMatch(Path.GetFileName(x), "^\d+"))
    |> Seq.choose
        (function
        | ExeDrop fp ->
            if fp.Contains("Debug") then
                printfn "Found a exeDrop at %s" fp
            Some fp
        | ZipDrop fp -> 
            printfn "Found zipDrop %s" fp
            Some fp
        | d ->
                let files = Directory.GetFiles d
                if files.Length > 0 then
                    searchedButEmpty.Add((d, files))
                None
    )
    |> Seq.sort
    |> List.ofSeq
    |> List.rev
    
let getDropInfo fp () =
    let fi = FileInfo(fp)
    let fvi = FileVersionInfo.GetVersionInfo fp
    fp,fvi.FileVersion,fi.CreationTime, fi.LastWriteTime
let cacheDropInfo path =
    try
        Util.Cache(Func<_>(getDropInfo path),path)
    with ex ->
        printfn "Failing on %s" path
        reraise()
targets
|> List.map findDrops
|> List.map(List.map cacheDropInfo)
|> Dump
|> ignore
searchedButEmpty.Dump("maybe errors or missed items?")