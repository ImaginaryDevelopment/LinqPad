<Query Kind="FSharpProgram" />

type FilePath = FilePath of string
// http://www.ark-survival.net/en/game-ini-modification-tutorial/
type StatMult = 
    | Health = 0
    | Stamina = 1
    | Torpidity = 2
    | Oxygen = 3
    | Food = 4
    | Water = 5
    | Temperature = 6
    | Weight = 7
    | MeleeDamage = 8
    | Speed = 9
    | TempFort = 10
    | CraftSpeed = 11
let containsI (d:string)(x:string) = x.Contains(d,StringComparison.CurrentCultureIgnoreCase)
let getArkFolders() =
    ([],Environment.GetLogicalDrives())
    ||> Seq.fold(fun found x ->
        [
            "Steam"
            @"Games\"
        ]
        |> List.choose(fun subdir ->
            let sd = Path.Combine(x,subdir, "steamapps\common\ARK")
            if Directory.Exists sd then
                Some (Path.Combine(sd, "ShooterGame\Saved\Config\WindowsNoEditor"))
            else None
        )
        |> ( @ ) found
                
    )
let targets =
    [
        yield Path.Combine(
            Path.GetDirectoryName (
                Environment.ExpandEnvironmentVariables @"%appdata%"), 
                @"Local\Packages\StudioWildcard.4558480580BB9_1w2mm55455e38\LocalState\Saved\UWPConfig\UWP")
        yield! getArkFolders()
        
    ]
type SettingType = | Raw of string | TF of bool            
type GameIni = | GameIni of string * string
type Setting =
    | Game of GameIni

type SettingMap = Map<FilePath,Setting>
//let writeSettings setting p =
//    match setting with
//    | Game (GameIni(key,value)) ->
        
let interested (x:string) : bool =
    [
        containsI "AlwaysAllowStructurePickup" // gameusersettings.ini
        containsI "flyer"
        containsI "tether"
    ]
    |> Seq.exists(fun f -> f x)
targets
|> Seq.iter(fun target ->
    Directory.EnumerateFiles(target,"*.ini")
    |> fun x -> x.Dump(); x
    //|> Seq.filter(fun x -> x.Contains("Game"))
    |> Seq.map(fun x -> Path.GetFileName x, File.ReadAllLines x, x)
    |> Seq.map(fun (fn,l, fp) -> fn,l |> Seq.filter interested, fp)
    |> Dump
    |> ignore
)