<Query Kind="FSharpProgram" />

// find PoE chat log, search for text perhaps

let leagueStartOpt = None // Some "2020/04/01 12"
let debug = false

[<Struct>]
type SearchType =
    | Single of term:string
    | And of terms:string list
    
let searchTerms = [
    And [ "anguish"; "@To"]
    //Single "@To"
    //Single "@From"
    ]

module LogLocation =
    let regularSearch (rootDirectory:DirectoryInfo) =
        [
            yield rootDirectory.FullName
            yield Path.Combine(rootDirectory.FullName,"Games")
            yield Path.Combine(rootDirectory.FullName,"games")
        ]
        |> Seq.filter(Directory.Exists)
        
    // for steam users they should be in the steam game installation folder
    let steamSearch (rootDirectory:DirectoryInfo) =

        let commonSteamFolderNames = [
            "Steam"
            "SteamLibrary"
        ]
        [
        
            yield! commonSteamFolderNames |> Seq.map(fun x -> Path.Combine(rootDirectory.FullName, x))
            yield! commonSteamFolderNames |> Seq.map(fun x -> Path.Combine(rootDirectory.FullName,"Games", x))
            yield! commonSteamFolderNames |> Seq.map(fun x -> Path.Combine(rootDirectory.FullName,"games", x))
        ]
        |> Seq.map(fun root -> Path.Combine(root,"steamapps","common"))
        |> Seq.filter(Directory.Exists)
    
let inline throwNullOrEmpty name =
    function
    | null | "" -> invalidOp <| sprintf "%s must not be null or empty" name
    | _ -> ()
    
let inline withValueString f =
    function
    | null | "" -> None
    | x -> f x |> Some

let inline startsWith search =
    throwNullOrEmpty "search" search
    withValueString(fun value -> value.StartsWith search)
    >> Option.defaultValue false
    
let inline contains search = 
    throwNullOrEmpty "search" search
    withValueString(fun value -> value.Contains search)
    >> Option.defaultValue false
    
let inline containsI search value = 
    throwNullOrEmpty "search" search
    match value with
    | null | "" -> false
    | value -> value.Contains(search,StringComparison.InvariantCultureIgnoreCase)

let streamLines (fp:string) =
    seq{
        use stream = new FileStream(fp,FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
        use sr = new StreamReader(stream)
        yield!
            sr
            |> Seq.unfold(fun sr ->
                if sr.EndOfStream then
                    None
                else
                    Some(sr.ReadLine(),sr)
            )
    }
System.IO.DriveInfo.GetDrives()
|> Seq.filter(fun d ->
    d.DriveType = System.IO.DriveType.Fixed
)
|> Seq.map(fun x -> x.RootDirectory)
|> Seq.collect(fun rd ->
    [
        LogLocation.steamSearch rd
        LogLocation.regularSearch rd
    ]
    |> Seq.collect id
)
|> List.ofSeq
|> List.filter(Directory.Exists)
|> fun dirs ->
    if debug then
        printfn "Searching %i root dirs" dirs.Length
        dirs.Dump("Searching")
    dirs
|> Seq.collect(fun root ->
    [
        "Path of Exile"
        "PathOfExile"
    ]
    |> Seq.map(fun p -> Path.Combine(root,p, "logs"))
)
//|> Seq.filter(Directory.Exists)
|> List.ofSeq
|> fun dirs ->
    if debug then
        printfn "Searching %i dirs" dirs.Length
        dirs.Dump("Searching logpaths")
    dirs
|> Seq.map(fun logPath ->
    if debug then printfn "%s, exists? %b" logPath <| Directory.Exists logPath 
    let p = Path.Combine(logPath,"Client.txt")
    if debug then
        printfn "Checking if log %s exists, %b" p <| File.Exists p
    p
)
|> Seq.filter(File.Exists)
|> Seq.distinct
|> Seq.map(fun logPath ->
    printfn "Getting log file info"
    let fi = FileInfo(logPath)
    fi,fi.LastWriteTime
)
|> Seq.sortByDescending snd

|> fun x ->
    if debug then
        x.Dump("logs to search")
    x
|> Seq.tryHead
|> Option.iter(fun (fi, _dt) ->
    GC.GetGCMemoryInfo().Dump("before")
    use fr = File.Open(fi.FullName,FileMode.Open,FileAccess.Read,FileShare.ReadWrite)
    use sr = new StreamReader(fr)
    sr
    |> Seq.unfold(fun sr ->
        if sr.EndOfStream then None
        else Some(sr.ReadLine(),sr)
    )
    |> fun x -> // if we have a league start, skip all before that 
        // we are doing raw string skipping here instead of an actual time. 
        // if the journal starts after the specified date/time it might skip all
        leagueStartOpt
        |> Option.map(fun ls -> 
            x |> Seq.skipWhile(startsWith ls >> not) //fun x -> not <| x.StartsWith ls)
        )
        |> Option.defaultValue x
    //|> Seq.skip 1
    //|> Seq.skipWhile(contains "Connect time" >> not)
    //|> Seq.skip 1
    //|> Seq.skipWhile(contains "Connect time" >> not)
    //|> Seq.skip 1
    //|> Seq.skipWhile(contains "Connect time" >> not)
    //|> Seq.skip 1
    |> Seq.filter(fun x -> not <| x.Contains("[VULKAN]"))
    |> Seq.filter(fun x -> not <| x.Contains("[EOS SDK]"))
    |> fun x ->
        match searchTerms with
        | [] -> x
        | _ ->
            x |> Seq.filter(fun y ->
                searchTerms
                |> Seq.exists(
                    function
                    | Single t -> containsI t y
                    | And ts -> (true,ts) ||> Seq.fold(fun b term -> b && containsI term y)
                )
            )
    |> Seq.truncate 200
    |> Dump
    |> ignore
)