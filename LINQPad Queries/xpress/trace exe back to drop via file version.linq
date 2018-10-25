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
    let exeSubPath = @"drop\PM\" + exeName
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
    let (|NullableValue|NullableNull|) (x:_ Nullable) =
        if x.HasValue then
            NullableValue x.Value
        else NullableNull
        
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
    module File =
        let tryReadLines f =
            if File.Exists f then
                File.ReadAllLines f |> Some
            else None
        let writeLines p (contents:string seq) =
            File.WriteAllLines(p,contents)
    let (|FileExists|_|) filePath =
        if File.Exists filePath then
            Some filePath
        else None
    let (|CombineFileExists|_|) subPath parentPath =
        match Path.Combine(parentPath,subPath) with
        | FileExists x -> Some x
        | x -> 
            printfn "NotFound:%s" x
            None
    ()
()    
        
open Helpers
type DropRoot = |DropRoot of string with
    override x.ToString() = match x with | DropRoot x -> x
    member private x.ToDump() = x.ToString()

module BuildMetaText =
    type BuildTrace = {Path:string;FileVersion:string;DropRoot:DropRoot;Creation:DateTime Nullable; MetaType:string}
    let getBuildTextPath (DropRoot d) = Path.Combine(d,"drop","build.txt")
    let tryGetCreationDate text =
        let matchFileVersion =
            function 
            //  Tue 10/02/2018 14:55:44.07 
            |RMatchGroup (@"^\w+ (\d{1,2}/\d{1,2}/\d{4}.*)",1) (ParseDateTime dt) -> Some dt
            | _ -> None
        let tryFindMatch f =
            text
            |> Seq.choose f
            |> Seq.tryHead
        tryFindMatch matchFileVersion
    
    let (|CreationInfo|_|) text = tryGetCreationDate text
    
    let tryInsertCreationDate (buildTextPath:string) (creationDate:DateTime) =
        if not <| buildTextPath.EndsWith("build.txt",StringComparison.InvariantCultureIgnoreCase) then failwithf "insertion called on wrong file type '%s'" buildTextPath
        // matches what is already standard in build text files
        let cTxt = creationDate.ToString("ddd MM/dd/yyyy HH:mm:ss.ff")
        // this isn't checking one already exists?
        File.tryReadLines buildTextPath
        |> function
            |Some (CreationInfo _ ) ->
                None
            |Some lines ->
                Some <| List.ofArray lines
            |None -> Some []
        |> Option.iter(
            function
            | fv::pv::sv::d::tl ->
                fv::pv::sv::d::cTxt::tl
            | [] -> [cTxt]
            | l -> List.append l [cTxt]
            >> File.writeLines buildTextPath
            >> (fun () -> printfn "Updated or created (%s) %s" cTxt buildTextPath)
        )
                
    
    let (|FileVersionInfo|_|) text =
        let matchFileVersion =
            function 
            |RMatchGroup ("FileVersion: (.*)",1) v -> Some v
            | _ -> None
        let tryFindMatch f =
            text
            |> Seq.choose f
            |> Seq.tryHead
        tryFindMatch matchFileVersion
    
    // would have been nice to include Creation (assuming the extraction's dt info is accurate)
    let insertMetaText (DropRoot d) bt =
        let fp = getBuildTextPath (DropRoot d)
        if not <| String.IsNullOrWhiteSpace bt.FileVersion then
            let fvTxt = sprintf "FileVersion: %s" bt.FileVersion 
            File.tryReadLines d
            |> function
                |Some (FileVersionInfo _) ->
                    None
                | Some lines ->
                    Some <| List.ofArray lines
                | None -> Some []
            |> Option.iter(
                function
                | h::tl -> h :: fvTxt :: tl
                | [] -> [fvTxt]
                >> File.writeLines fp
                >> (fun () -> printfn "Updated or created (%s) %s" fvTxt fp)
            )
        
    let (|ParseBuildText|_|) text =
        let matchFileVersion =
            function 
            |RMatchGroup ("FileVersion: (.*)",1) v -> Some v
            | _ -> None
        let matchCreation =
            function
            |RMatchGroup (@"^\w+ (\d{1,2}/\d{1,2}/\d{4}.*)",1) (ParseDateTime dt) -> Some dt
            | _ -> None
        let tryFindMatch f =
            text
            |> Seq.choose f
            |> Seq.tryHead
        tryFindMatch matchFileVersion
        |> Option.map(fun x -> x,tryFindMatch matchCreation)
        
    // if there is a build.txt and it has VersionInfo then this one should win
    let (|BuildTxtDrop|_|) d =
        let fp = getBuildTextPath d
        match File.tryReadLines fp with
        | Some (ParseBuildText (fv,creation)) ->
            Some {Path=fp;DropRoot=d;FileVersion = fv;Creation = creation|> Option.toNullable;MetaType="BuildTxt"}
        | Some bp ->
//            printfn "No version info or unreadable buildTxt:%s" fp
            None
        |None -> None
    ()
    
open BuildMetaText    

module LegacyDrops =
    type LegacyDrop = {DropRoot:DropRoot;ExePath:string}
        
    let getDropInfo (fp,typ,d) () =
        let fi = FileInfo(fp)
        let fvi = FileVersionInfo.GetVersionInfo fp
        {Path=fp;DropRoot=d;FileVersion=fvi.FileVersion;Creation=Nullable fi.CreationTime;MetaType=typ}
    // search around
    let tryFindExe (DropRoot d) metaDirectory =
        match d with
        | CombineFileExists exeSubPath fp ->
            Some {DropRoot=DropRoot d;ExePath=fp}
        | CombineFileExists (sprintf "drop/%s" exeName) fp ->
            Some {DropRoot=DropRoot d;ExePath=fp}
        | _ ->
            failwithf "uhoh! %s was drop root" d
        
    let (|ExeDrop|_|) (DropRoot d) = 
        let fp = Path.Combine(d,exeSubPath)
        if File.Exists fp then
            Some {DropRoot=DropRoot d;ExePath=fp}
        else 
            try
                let extractedP = Path.Combine(d,"drop",exeName) 
                if File.Exists extractedP then
                    Some {DropRoot=DropRoot d;ExePath=extractedP}
                else 
                    None
            with ex ->
                printfn "EXE Drop failing"
                reraise()
    // assertion: zip drops should theoretically only ever be future style where unzipping shouldn't be needed
    let (|ZipDrop|_|) (DropRoot d) =
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
    |BuildPath of LegacyDrop*matchType:string

let findDrops p = 
    let d =  Directory.EnumerateDirectories p |> List.ofSeq
    printfn "Searching %i directories under %s (%i unique)" d.Length p (d |> List.distinct |> List.length)
    d
    |> List.filter(fun x -> Regex.IsMatch(Path.GetFileName(x), "^\d+"))
    |> List.map (DropRoot)
    |> Seq.choose
        (function
        |BuildTxtDrop x ->
            if debug then
                printfn "Found a buildTxtDrop at %s %A" x.Path x
            Some <| BuildText x
        | ExeDrop fp ->
            Some <| BuildPath (fp,"exe")
        | ZipDrop fp as d ->
            if debug then
                printfn "Found zipDrop %s" fp
            Some <| BuildPath ({DropRoot=d;ExePath=fp},"zip")
        | DropRoot d ->
                let files = Directory.GetFiles d
                if files.Length > 0 then
                    searchedButEmpty.Add((d, files))
                None
    )
    |> List.ofSeq
    
let cacheDropInfo i =
    function
    | BuildText x -> x
    | BuildPath ({DropRoot=d;ExePath=exePath},meta) ->
        try
            Util.Cache(Func<_>(getDropInfo (exePath,meta,d)),exePath)
        with ex ->
            printfn "Failing on %s" exePath
            reraise()
        |> fun bt ->
            insertMetaText d bt
            bt
    >> fun bt ->
        // we need this path to actually pipe bt through
        // so if a creation is found/updated, bt is rebuilt with the new information, instead of having to run this script again
        let metaPath = getBuildTextPath bt.DropRoot
        match bt.Creation, File.tryReadLines metaPath |> Option.bind BuildMetaText.tryGetCreationDate with
        | NullableValue creation,None -> Some creation
        // no creation date, search around for an extracted exe or flat drop
        | NullableNull,None ->
            Path.GetDirectoryName metaPath
            |> tryFindExe bt.DropRoot
            |> Option.bind(fun ld -> 
                let di = getDropInfo(ld.ExePath,"creationism",ld.DropRoot) ()
                di.Creation
                |> Option.ofNullable
            )
            
        | _ -> None
        |> Option.iter(tryInsertCreationDate metaPath)
        bt
            
        
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