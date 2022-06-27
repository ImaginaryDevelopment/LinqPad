<Query Kind="FSharpProgram" />

// show default values, steam settings, and xbox for diffing

let steamPath = @"D:\Games\steamapps\common\ARK\ShooterGame\Saved\Config\WindowsNoEditor"
let xboxPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), @"Packages\StudioWildcard.4558480580BB9_1w2mm55455e38\LocalState\Saved\UWPConfig\UWP\")

module Helpers =
    let failNullOrEmpty =
        function
        | null -> failwith "Null is not allowed"
        | "" -> failwith "Empty is not allowed"
        | _ -> ()
    let indexOf (delim:string) (x:string) =
        failNullOrEmpty delim
        match x.IndexOf(delim) with
        | i when i < 0 -> None
        | i -> Some i
        
    // says try, but will throw on bad delim
    let tryAfter delim x =
        x |> indexOf delim
        |> Option.map(fun i ->
            x.[i + delim.Length .. ]
        )
    let after delim x =
        tryAfter delim x
        |> function
            | Some v -> v
            | None -> failwithf "Delimiter not found: '%s' in string of length %i" delim (x.Length)
    let tryBefore delim x =
        x
        |> indexOf delim
        |> Option.map(fun i ->
            x.[0 .. i - 1]
        )
        
    let before delim x =
        tryBefore delim x
        |> function
            | Some v -> v
            | None -> failwithf "Delimiter not found: '%s' in string of length %i" delim (x.Length)
open Helpers

type Stat = 
    | Health
    | StaminaOrCharge
    | Torpidity
    | OxygenOrChargeGen
    | Food
    | Water
    | Temperature
    | Weight
    | MeleeDmgOrChargeRange
    | SpeedMultiplier
    | TemperatureFortitude
    | CraftingSpeedMultiplier
    with
        static member GetValue s =
            match s with
            | Health -> 0
            | StaminaOrCharge -> 1
            | Torpidity -> 2
            | OxygenOrChargeGen -> 3
            | Food -> 4
            | Water -> 5
            | Temperature -> 6
            | Weight -> 7
            | MeleeDmgOrChargeRange -> 8
            | SpeedMultiplier -> 9
            | TemperatureFortitude -> 10
            | CraftingSpeedMultiplier -> 11
        static member GetAll () =
            [
                Health
                StaminaOrCharge
                Torpidity
                OxygenOrChargeGen
                Food
                Water
                Temperature
                Weight
                MeleeDmgOrChargeRange
                SpeedMultiplier
                TemperatureFortitude
                CraftingSpeedMultiplier
            ]
            
type StatType =
    | Wild
    | Tamed
    | Tamed_Add
    | Tamed_Affinity
    with
        static member GetAll() =
            [
                Wild
                Tamed
                Tamed_Add
                Tamed_Affinity
            ]
    
let getSinglePlayerMult statType stat =
    match stat,statType with
    | Health, Tamed -> 2.125
    | MeleeDmgOrChargeRange, Tamed -> 2.353
    | MeleeDmgOrChargeRange, Tamed_Add
    | Health, Tamed_Add -> 3.571
    | MeleeDmgOrChargeRange, Tamed_Affinity
    | Health, Tamed_Affinity -> 2.273
    | _ -> 1.0
    
// most values are 1 by default, all known deviations should be accounted for here
// unsure if the lack of Water, Temperature, TemperatureFortitude, CraftingSpeedMultiplier should be modeled as None, or default 1.0
let getDefaultStatMult statTypeOpt useSinglePlayer stat =
    let spChange =
        if useSinglePlayer then
            statTypeOpt |> Option.map(fun st -> getSinglePlayerMult st stat)
        else None
        |> Option.defaultValue 1.0
    // stateTypeOpt = None -> Player
    match stat, statTypeOpt with
    | Health, Some Tamed -> 0.2
    | Health, Some Tamed_Add -> 0.14
    | Health, Some Tamed_Affinity -> 0.44
    | MeleeDmgOrChargeRange, Some Tamed -> 0.17
    | MeleeDmgOrChargeRange, Some Tamed_Add -> 0.14
    | MeleeDmgOrChargeRange, Some Tamed_Affinity -> 0.44
    | _ -> 1.0
    |> (*) spChange
    
// Game.ini settings
type GameSetting =
    | PerLevelStatsMultiplier of Stat * StatType option
    | MatingIntervalMultiplier
    | MatingSpeedMultiplier
    | EggHatchSpeedMultiplier

    | BabyMatureSpeedMultiplier
    | BabyFoodConsumptionSpeedMultiplier

    | CropGrowthSpeedMultiplier
    | LayEggIntervalMultiplier
    | CustomRecipeEffectivenessMultiplier
    | CustomRecipeSkillMultiplier
    | DinoHarvestingDamageMultiplier
    | PlayerHarvestingDamageMultiplier
    | DinoTurretDamageMultiplier
    
    | SupplyCrateLootQualityMultiplier
    | FishingLootQualityMultiplier
    
    | BabyImprintingStatScaleMultiplier
    | BabyCuddleIntervalMultiplier
    | BabyCuddleGracePeriodMultiplier
    | BabyCuddleLoseImprintQualitySpeedMultiplier
    | BabyImprintAmountMultiplier
    
    | TamedDinoCharacterFoodDrainMultiplier
    | WildDinoCharacterFoodDrainMultiplier
    | WildDinoTorporDrainMultiplier
    | PassiveTameIntervalMultiplier
    | TamedDinoTorporDrainMultiplier
    | KillXPMultiplier
    | HarvestXPMultiplier
    | CraftXPMultiplier
    | GenericXPMultiplier
    | SpecialXPMultiplier
    | FuelConsumptionIntervalMultiplier
    | CraftingSkillBonusMultiplier
    with
        static member GetAll() = 
            [
                for s in Stat.GetAll() do
                    PerLevelStatsMultiplier(s, None)
                    for st in StatType.GetAll() do
                        PerLevelStatsMultiplier(s, Some st)
                    
                MatingIntervalMultiplier
                MatingSpeedMultiplier
                EggHatchSpeedMultiplier
                
                BabyMatureSpeedMultiplier
                BabyFoodConsumptionSpeedMultiplier
                
                CropGrowthSpeedMultiplier
                LayEggIntervalMultiplier
                CustomRecipeEffectivenessMultiplier
                CustomRecipeSkillMultiplier
                DinoHarvestingDamageMultiplier
                PlayerHarvestingDamageMultiplier
                DinoTurretDamageMultiplier
                
                SupplyCrateLootQualityMultiplier
                FishingLootQualityMultiplier
                
                BabyImprintingStatScaleMultiplier
                BabyImprintAmountMultiplier
                BabyCuddleIntervalMultiplier
                BabyCuddleGracePeriodMultiplier
                BabyCuddleLoseImprintQualitySpeedMultiplier
                
                TamedDinoCharacterFoodDrainMultiplier
                WildDinoCharacterFoodDrainMultiplier
                WildDinoTorporDrainMultiplier
                PassiveTameIntervalMultiplier
                KillXPMultiplier
                HarvestXPMultiplier
                CraftXPMultiplier
                GenericXPMultiplier
                SpecialXPMultiplier
                FuelConsumptionIntervalMultiplier
                CraftingSkillBonusMultiplier
            ]

type GameUserSetting =
    | DifficultyOffset
    | DinoDamageMultiplier
    | PlayerDamageMultiplier
    | PlayerResistanceMultiplier
    | DinoResistanceMultiplier
    | TamingSpeedMultiplier
    | HarvestAmountMultiplier
    | HarvestHealthMultiplier
    | ResourcesRespawnPeriodMultiplier
    | PlayerCharacterWaterDrainMultiplier
    | PlayerCharacterFoodDrainMultiplier
    | PlayerCharacterStaminaDrainMultiplier
    | PlayerCharacterHealthRecoveryMultiplier
    | DinoCharacterFoodDrainMultiplier
    | DinoCharacterStaminaDrainMultiplier
    | DinoCharacterHealthRecoveryMultiplier
    | DinoCountMultiplier
    with
        static member GetAll() =
            [
                DifficultyOffset
                DinoDamageMultiplier
                PlayerDamageMultiplier
                PlayerResistanceMultiplier
                DinoResistanceMultiplier
                TamingSpeedMultiplier
                HarvestAmountMultiplier
                HarvestHealthMultiplier
                ResourcesRespawnPeriodMultiplier
                PlayerCharacterWaterDrainMultiplier
                PlayerCharacterFoodDrainMultiplier
                PlayerCharacterStaminaDrainMultiplier
                PlayerCharacterHealthRecoveryMultiplier
                DinoCharacterFoodDrainMultiplier
                DinoCharacterStaminaDrainMultiplier
                DinoCharacterHealthRecoveryMultiplier
                DinoCountMultiplier
            ]
    
type SettingType =
    | GameIni of GameSetting
    | GameUserSetting of GameUserSetting
    
type SettingSource =
    | DefaultValue
    | Steam
    | Xbox
    
module GameDefaults =    
    let getDefaultSetting =
        function
        | GameIni gs ->
            match gs with
            |  PerLevelStatsMultiplier(stat, statTypeOpt) ->
                // do not use SP values, we are getting default settings not the effective end result value
                getDefaultStatMult statTypeOpt false stat
            | _ -> 1.0
        
        | GameUserSetting DifficultyOffset -> 0.2
        | GameUserSetting x ->
            match x with
            | DinoDamageMultiplier
            | PlayerDamageMultiplier
            | PlayerResistanceMultiplier
            | DinoResistanceMultiplier
            | TamingSpeedMultiplier
            | HarvestAmountMultiplier
            | HarvestHealthMultiplier
            | ResourcesRespawnPeriodMultiplier
            | PlayerCharacterWaterDrainMultiplier
            | PlayerCharacterFoodDrainMultiplier
            | PlayerCharacterStaminaDrainMultiplier
            | PlayerCharacterHealthRecoveryMultiplier
            | DinoCharacterFoodDrainMultiplier
            | DinoCharacterStaminaDrainMultiplier
            | DinoCharacterHealthRecoveryMultiplier
            | DinoCountMultiplier
                -> 1.0
            | x -> failwithf "getDefaultSetting.GameUserSetting: Match cases incomplete: missing %A" x
    
let getSettingLinePrefix =         
    function
    | GameIni (PerLevelStatsMultiplier(stat, statTypeOpt)) ->
        sprintf "PerLevelStatsMultiplier_%s[%i]=" (statTypeOpt |> Option.map(string>>sprintf"Dino%s") |> Option.defaultValue "Player") (Stat.GetValue stat)
    | GameIni x ->
        string x |> sprintf "%s="
    | GameUserSetting x ->
        sprintf "%A=" x
    
let getSettingFromFile fp =
    let text = File.ReadAllLines(fp)
    function
    | GameIni _ as gs  ->
        let lineStart = getSettingLinePrefix gs
        text
        |> Seq.tryFind(fun line -> line.StartsWith lineStart)
        |> Option.map(after "=" >> float)
        |> Option.defaultValue -1.0
    | GameUserSetting _ as gs ->
        let lineStart = getSettingLinePrefix gs
        text
        |> Seq.tryFind(fun line -> line.StartsWith lineStart)
        |> Option.map(after "=" >> float)
        |> Option.defaultValue -1.0
        
let getSetting ss gs =
    match ss,gs with
    | DefaultValue, _ ->
        GameDefaults.getDefaultSetting gs
    | Steam, GameIni _ ->
        let path = Path.Combine(steamPath, "Game.ini")
        getSettingFromFile path gs
    | Xbox, GameIni _ ->
        let path = Path.Combine(xboxPath, "Game.ini")
        getSettingFromFile path gs
    | Steam, GameUserSetting _ ->
        let path = Path.Combine(steamPath, "GameUserSettings.ini")
        getSettingFromFile path gs
    | Xbox, GameUserSetting  _ ->
        let path = Path.Combine(xboxPath, "GameUserSettings.ini")
        getSettingFromFile path gs
        
type StatComparison = {
    Name:string
    DefaultValue: float
    SteamValue: float
    XboxValue: float
    LinePrefix: string
}

let allInterestedValues =
    [
        yield! GameUserSetting.GetAll() |> List.map GameUserSetting
        yield! GameSetting.GetAll() |> List.map GameIni
    ]
    |> List.map(fun gs ->
        let d = getSetting DefaultValue gs
        // assume the one farther from default is easiest?
        let s = getSetting Steam gs
        let x = getSetting Xbox gs
        //let easiest = [s;x] |> List.maxBy(fun v -> v - d |> abs)
        {Name=string gs;DefaultValue= d; SteamValue= s;XboxValue= x;LinePrefix=getSettingLinePrefix gs |> before "=" }
    )
    // filter out things that shouldn't exist
    |> List.filter(fun gs ->
        gs.Name.Contains("CraftingSpeedMultiplier, Some") |> not
    )
    
// make sure all values are distinct
(
    let getDupes f = allInterestedValues |> List.map f |> List.countBy id |> List.filter(fun (_,c) -> c <> 1)
    let namebad = getDupes (fun gs -> gs.Name)
    let linebad = getDupes (fun gs -> gs.LinePrefix)
    if namebad.Length > 0 then
        namebad.Dump("bad")
    if linebad.Length > 0 then
        linebad.Dump("bad")
)
type DiffInterestType =
    | UnmatchedCustom
    | NonDefault
    | NotFound
    | Def

let getIntType gs =
    if gs.SteamValue <> gs.XboxValue then
        UnmatchedCustom
    elif gs.SteamValue = -1.0 && gs.XboxValue = -1.0 then
        NotFound
    elif gs.SteamValue <> gs.DefaultValue || gs.XboxValue <> gs.DefaultValue then
        NonDefault
    else Def
    
//let isInteresting gs =
//   (gs.SteamValue <> gs.XboxValue && (gs.SteamValue <> -1.0 || gs.XboxValue <> -1.0))
//   && (gs.DefaultValue <> gs.SteamValue || gs.DefaultValue <> gs.XboxValue)
// filter all the ones that aren't customized in either?
allInterestedValues
|> List.groupBy getIntType
|> List.sortByDescending (fun (g,_) -> g = UnmatchedCustom, g = NonDefault, g = NotFound)
|> List.map(fun (g,items) -> string g, items)
|> Dump
|> ignore
