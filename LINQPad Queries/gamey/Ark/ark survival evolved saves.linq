<Query Kind="FSharpProgram" />

type FilePath = FilePath of string
let (|SteamSave|_|) x =
    if String.IsNullOrWhiteSpace x then None
    elif x.EndsWith".ark" then Some x
    // backups are > 30mb and end in Backup.bak
    elif x.EndsWith "Backup.bak" then Some x
    else None
type SaveType =
    | Steam
    | Xbox
    
let containsI (d:string)(x:string) = x.Contains(d,StringComparison.CurrentCultureIgnoreCase)

// https://www.pcgamesn.com/xbox-game-pass-pc-steam
let getWinArkFolders() = // xbox/gamepass/windows store
    // examples:
    //   C:\Users\<...>\AppData\Local\Packages\StudioWildcard.4558480580BB9_1w2mm55455e38\SystemAppData\wgs
    //   C:\Users\<...>\AppData\Local\Packages\StudioWildcard.ARKAberration_1w2mm55455e38 ???
    //   C:\Users\<...>\AppData\Local\Packages\StudioWildcard.ARKScorchedEarthGamePreview_1w2mm55455e38\SystemAppData ???
    let packages = Path.Combine(Path.GetDirectoryName(Environment.ExpandEnvironmentVariables @"%appdata%"),@"Local\Packages\")
    System.IO.Directory.EnumerateDirectories(packages, "*StudioWildCard*")
    |> Seq.collect(fun x -> Directory.EnumerateDirectories(x,"*wgs*", SearchOption.AllDirectories))

let getArkFolders() =
    // examples:
    //  D:\Games\steamapps\common\ARK\ShooterGame\Saved\Aberration_PSavedArksLocal
    //  E:\games\steamapps\common\ark\ShooterGame\Saved\SavedArksLocal
    ([],Environment.GetLogicalDrives())
    ||> Seq.fold(fun found x ->
        [
            "Steam"
            @"Games\"
        ]
        |> List.choose(fun subdir ->
            let sd = Path.Combine(x,subdir, "steamapps\common\ARK")
            if Directory.Exists sd then
                Some (Path.Combine(sd, "ShooterGame\Saved"))
            else None
        )
        |> ( @ ) found
    )
let targets =
    [
                //@"StudioWildcard.4558480580BB9_1w2mm55455e38\LocalState\Saved")
        yield! getWinArkFolders() |> Seq.map(fun w -> Xbox, w)
        let ark = getArkFolders()
        yield! ark |> Seq.map(fun w -> Steam, w)
        
    ]
targets
|> Seq.map(fun (t,d) ->
    match t with
    | Steam -> string t,d,Directory.EnumerateFiles(d,"*.ark",SearchOption.AllDirectories)
    | Xbox -> string t, d , Seq.empty
    //| Xbox -> string t, d, Directory.EnumerateFiles(d, "
)
|> Dump