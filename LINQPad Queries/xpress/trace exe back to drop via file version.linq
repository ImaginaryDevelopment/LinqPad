<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.IO.Compression.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.IO.Compression.FileSystem.dll</Reference>
  <Namespace>System.IO.Compression</Namespace>
  <DisableMyExtensions>true</DisableMyExtensions>
  <CopyLocal>true</CopyLocal>
</Query>

// iterate a build drop structure, getting a specific file's version info
// consider: adding the ability to query changeset number
// took ~2 min 2 process 137 items
module Settings =
    let allowUnzipping = true
    let debug = false
    let exeName = "PracticeManagement.exe"
    let subPath = @"drop\PM\" + exeName
    let targetish = 
        let env =Environment.ExpandEnvironmentVariables("%droproot%")
        if String.IsNullOrWhiteSpace env then
            let env = Util.ReadLine(@"Drop root? (\\server\share\path)")
            if not <| String.IsNullOrWhiteSpace env && Directory.Exists env then
                Environment.SetEnvironmentVariable("droproot",env,EnvironmentVariableTarget.User)
                env
            else failwithf "No drop root provided or found"
        else env
    let targetSubDirectories = ["Pm_VsBuild_Release_Deploy";"Pm_VsBuild_Deploy/release"]
    printfn "Targetish: %s" targetish
open Settings    

module Helpers =
    let dprintfn f =
        Printf.kprintf( fun s -> if debug then printfn "%s" s) f
        
    let toCharArray(x:string) = x.ToCharArray()
    let trimStart1 d (x:string) = x.TrimStart(d |> toCharArray)
    let (@@) path subPath = 
        subPath 
        |> trimStart1 "\\"
        |> trimStart1 "/"
        |> fun x -> Path.Combine(path,subPath)
    let (|RMatch|_|) p x=
        let m = Regex.Match(x,p)
        if m.Success then Some m else None
    let inline (|Parse|_|) f x =
        match f x with
        | true, x -> Some x
        | false, _ -> None
    let (|ParseDateTime|_|) =
        function
        | null | "" -> None
        | Parse DateTime.TryParse dt -> Some dt
        | _ -> None
    
        
    let (|RMatchGroup|_|) (p,i:int) =
        function
        | RMatch p m -> Some m.Groups.[i].Value
        | _ -> None
        
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
open Helpers

type BuildTrace = {Path:string;FileVersion:string;Creation:DateTime Nullable; MetaType:string}
// if there is a build.txt and it has VersionInfo then this one should win
let (|BuildTxtDrop|_|) d =
    let fp = Path.Combine(d,"drop","build.txt")
    let findFileVersion =
        function 
        |RMatchGroup ("FileVersion: (.*)",1) v -> Some v
        | _ -> None
    let getCreation =
        function
        |RMatchGroup (@"^\w+ (\d{1,2}/\d{1,2}/\d{4}.*)",1) (ParseDateTime dt) -> Some dt
        | _ -> None
    let findCreation =
        Seq.choose getCreation
        >> Seq.tryHead
    if File.Exists fp then
        let text = File.ReadAllLines fp
        text
        |> Seq.choose findFileVersion
        |> Seq.tryHead
        |> Option.map(fun x -> {Path=fp;FileVersion = x;Creation = findCreation text|> Option.toNullable;MetaType="BuildTxt"})
    else None
    
    // if there is a build.txt and it has VersionInfo then this one should win
module LegacyDrops =
    let getDropInfo (fp,typ) () =
        let fi = FileInfo(fp)
        let fvi = FileVersionInfo.GetVersionInfo fp
        {Path=fp;FileVersion=fvi.FileVersion;Creation=Nullable fi.CreationTime;MetaType=typ}
        
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
        if Directory.Exists zipPath && allowUnzipping then
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
open LegacyDrops        
let searchedButEmpty = ResizeArray()
        
type DropType =
    |BuildText of BuildTrace
    |BuildPath of string*matchType:string

let findDrops p = 
    let d =  Directory.EnumerateDirectories p |> List.ofSeq
    printfn "Searching %i directories under %s (%i unique)" d.Length p (d |> List.distinct |> List.length)
    d
    |> List.filter(fun x -> Regex.IsMatch(Path.GetFileName(x), "^\d+"))
    |> Seq.choose
        (function
        |BuildTxtDrop x ->
            if debug then
                printfn "Found a buildTxtDrop at %s %A" x.Path x
            Some <| BuildText x
        | ExeDrop fp ->
//            if fp.Contains("Debug") then
//                printfn "Found a exeDrop at %s" fp
            Some <| BuildPath (fp,"exe")
        // we need to account for the possibility that buildTxt came out, but doesn't have what we need
            if debug then
                printfn "Found zipDrop %s" fp
            Some <| BuildPath (fp,"zip")
        | d ->
                let files = Directory.GetFiles d
                if files.Length > 0 then
                    searchedButEmpty.Add((d, files))
                None
    )
//    |> Seq.sortBy(fun x -> x.Creation)
    |> List.ofSeq
//    |> fun x ->
//        "done with listing now to reverse"
//        |> Msg
//        |> showProgress
//        x
//    |> List.rev
    
let cacheDropInfo i =
//    showProgress <| sprintf "getting cache info for item %i" i
    function
    | BuildText x -> x
    | BuildPath (path,meta) ->
        try
            Util.Cache(Func<_>(getDropInfo (path,meta)),path)
        with ex ->
            printfn "Failing on %s" path
            reraise()
        
let targets = targetSubDirectories |> List.map(fun s -> targetish @@ s)
printfn "Checking in %A" targets
targets
|> List.map findDrops
|> fun x ->
    x
    |> List.sumBy(List.choose(function | BuildPath _ -> Some () | _ -> None) >> List.length) 
    |> dprintfn "done finding drops %i to process"
    if debug then 
        List.map(List.truncate 20) x
    else 
        x
|> List.map(List.mapi cacheDropInfo)
|> List.map(List.sortByDescending(fun x -> x.Creation.GetValueOrDefault() ))
|> fun x ->
    dprintfn "done getting cache info"
    x
|> Dump
|> ignore
searchedButEmpty.Dump("maybe errors or missed items?")