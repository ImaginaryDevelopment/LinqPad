<Query Kind="FSharpProgram" />

// find PoE chat log, search for text perhaps

let leagueStartOpt = Some "2020/09/18 12"

[<Struct>]
type SearchType =
    | Single of term:string
    | And of terms:string list
    
let searchTerms = [
    And [ "Replica"; "@To"]
    //Single "@To"
    //Single "@From"
    ]

// for steam users they should be in the steam game installation folder
let commonSteamFolderNames = [
    "Steam"
    "SteamLibrary"
]
let inline throwNullOrEmpty name =
    function
    | null | "" -> invalidOp <| sprintf "%s must not be null or empty" name
    | _ -> ()
    
let inline withValueString f =
    function
    | null | "" -> None
    | x -> f x |> Some

            //Seq.skipWhile(fun x -> not <| x.StartsWith ls)
//let inline doesnt f x = f x |> not

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
|> Seq.fold(fun state rd ->
    state @ [
        yield! commonSteamFolderNames |> Seq.map(fun x -> Path.Combine(rd.FullName, x))
        yield! commonSteamFolderNames |> Seq.map(fun x -> Path.Combine(rd.FullName,"Games", x))
    ]
    
) List.empty
|> List.filter(Directory.Exists)
|> Seq.map(fun root -> Path.Combine(root,"steamapps","common","Path of Exile", "logs"))
|> Seq.filter(Directory.Exists)
|> Seq.map(fun logPath -> Path.Combine(logPath,"Client.txt"))
|> Seq.filter(File.Exists)
|> Seq.map(fun logPath ->
    let fi = FileInfo(logPath)
    fi,fi.LastWriteTime
)
|> Seq.sortByDescending snd

|> fun x ->
    x.Dump()
    x
|> Seq.tryHead
|> Option.iter(fun (fi, _dt) ->
    GC.GetGCMemoryInfo().Dump("before")
    use fr = File.Open(fi.FullName,FileMode.Open,FileAccess.Read,FileShare.ReadWrite)
    use sr = new StreamReader(fr)
    sr
    |> Seq.unfold(fun sr ->
        if sr.EndOfStream then None
        else Some( sr.ReadLine(),sr)
    )
    |> fun x -> 
        leagueStartOpt
        |> Option.map(fun ls -> 
            x |> Seq.skipWhile(startsWith ls >> not) //fun x -> not <| x.StartsWith ls)
        )
        |> Option.defaultValue x
    |> Seq.skip 1
    |> Seq.skipWhile(contains "Connect time" >> not)
    |> Seq.skip 1
    |> Seq.skipWhile(contains "Connect time" >> not)
    |> Seq.skip 1
    |> Seq.skipWhile(contains "Connect time" >> not)
    |> Seq.skip 1
    |> Seq.filter(fun x -> not <| x.Contains("[VULKAN]"))
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