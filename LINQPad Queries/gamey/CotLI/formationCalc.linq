<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

type JC = Newtonsoft.Json.JsonConvert
module Helpers =
    let after (delim:string) (x:string) = x.Substring(x.IndexOf(delim) + delim.Length) 
    let before (delim:string) (x:string) = x.Substring(0,x.IndexOf delim)
    let patternFromEquality x b = if x = b then Some() else None
open Helpers

module Crusaders =
    type Tag = 
        | Dps | Support 
        | Human | Animal | Robot | Alien | Dragon | Leprechaun | Dwarf | Elf | Orc
        | Male | Female
        | GoldFinder
        | Clicker
        member x.ToDump() = 
            sprintf "%A" x
        static member FromRaw = 
            function
            | "dps" -> Dps | "support" -> Support
            | "human" -> Human
            | "animal" -> Animal
            | "robot" -> Robot
            | "male" -> Male | "female" -> Female
            | "alien" -> Alien
            | "dragon" -> Dragon
            | "leprechaun" -> Leprechaun
            | "dwarf" -> Dwarf
            | "gold" -> GoldFinder
            | "clicker" -> Clicker
            | "elf" -> Elf
            | "orc" -> Orc
            | x -> failwithf "Unexpected tag %s" x
            
        
    type Crusader = 
        {   DisplayName:string; 
            // abandoning/not implementing the 01a nomenclature or .. deriving it
            Id: byte
            Tags: Tag list
            Raw:obj}
        member private x.ToDump() = 
            let y = {x with Raw = x.Raw |> string}
            y
    let (|BushWhacker|_|) = patternFromEquality 1uy // The Bush Whacker
    let (|Jim|_|) = patternFromEquality 2uy // Jim the Lumberjack
    let (|Emo|_|) = patternFromEquality 3uy // Emo Werewolf
    let (|Sasha|_|) = patternFromEquality 4uy // Sasha the Fierce Warrior
    let (|Hermit|_|) = patternFromEquality 5uy // The Washed Up Hermit
    let (|Detective|_|) = patternFromEquality 6uy // Detective Kaine
    let (|Princess|_|) = patternFromEquality 7uy // The Princess
    let (|Natalie|_|) = patternFromEquality 8uy // Natalie Dragon
    let (|Jason|_|) = patternFromEquality 9uy // Jason, Master of Shadows
    let (|Artaxes|_|) = patternFromEquality 10uy // Artaxes, the Lion
    let (|Khouri|_|) = patternFromEquality 11uy // Khouri, the Witch Doctor
    let (|DarkGryphon|_|) = patternFromEquality 12uy // Dark Gryphon
    let (|Sarah|_|) = patternFromEquality 13uy // Sarah, the Collector
    let (|Panda|_|) = patternFromEquality 14uy // Gold Panda
    let (|Sal|_|) = patternFromEquality 15uy // Prince Sal, the Merman
    let (|Phoenix|_|) = patternFromEquality 16uy // Fire Phoenix
    let (|Reginald|_|) = patternFromEquality 17uy // King Reginald IV
    let (|Thalia|_|) = patternFromEquality 18uy // Thalia, the Thunder King
    let (|Merci|_|) = patternFromEquality 19uy // Merci, the Mad Wizard
    let (|Nate|_|) = patternFromEquality 20uy // Nate Dragon
    let (|Mister|_|) = patternFromEquality 21uy // Mister the Monkey
    let (|Pete|_|) = patternFromEquality 22uy // Pete the Carney
    let (|Wendy|_|) = patternFromEquality 23uy // Wendy the Witch
    let (|Jack|_|) = patternFromEquality 24uy // Jack O'Lantern
    let (|RoboTurkey|_|) = patternFromEquality 25uy // RoboTurkey
    let (|Momma|_|) = patternFromEquality 26uy // Momma Kaine
    let (|RoboSanta|_|) = patternFromEquality 27uy // RoboSanta
    let (|Frosty|_|) = patternFromEquality 28uy // Frosty the Snowman
    let (|Pam|_|) = patternFromEquality 29uy // Pilot Pam
    let (|Siri|_|) = patternFromEquality 30uy // Queen Siri
    let (|Groklok|_|) = patternFromEquality 31uy // Groklok the Orc
    let (|Drizzle|_|) = patternFromEquality 32uy // Drizzle the Dark Elf
    let (|Rocky|_|) = patternFromEquality 33uy // Rocky the Rockstar
    let (|Sally|_|) = patternFromEquality 34uy // Sally the Succubus
    let (|Larry|_|) = patternFromEquality 35uy // Larry the Leprechaun
    let (|Kyle|_|) = patternFromEquality 36uy // Kyle the Party Bro
    let (|Alan|_|) = patternFromEquality 37uy // Alan the ArchAngel
    let (|RoboRabbit|_|) = patternFromEquality 38uy // RoboRabbit
    let (|Bat|_|) = patternFromEquality 39uy // The Bat Billionaire
    let (|MetalSoldierette|_|) = patternFromEquality 40uy // The Metal Soldierette
    let (|Broot|_|) = patternFromEquality 41uy // Broot
    let (|Robbie|_|) = patternFromEquality 42uy // Robbie Raccoon
    let (|Leerion|_|) = patternFromEquality 43uy // Leerion, the Royal Dwarf
    let (|Brogon|_|) = patternFromEquality 44uy // Brogon, Prince of Dragons
    let (|Montana|_|) = patternFromEquality 45uy // Montana James
    let (|Draco|_|) = patternFromEquality 46uy // Serpent King Draco
    let (|Billy|_|) = patternFromEquality 47uy // President Billy Smithsonian
    let (|Kizlblyp|_|) = patternFromEquality 48uy // Kizlblyp, the Alien Traitor
    let (|Rayna|_|) = patternFromEquality 49uy // Ranger Rayna
    let (|Littlefoot|_|) = patternFromEquality 50uy // Littlefoot
    let (|Veronica|_|) = patternFromEquality 51uy // Veronica, the Android Archer
    let (|Bubba|_|) = patternFromEquality 52uy // Bubba, the Swimming Orc
    let (|Karen|_|) = patternFromEquality 53uy // Karen, the Cat Teenager
    let (|Boggins|_|) = patternFromEquality 54uy // Mr. Boggins, the Substitute
    let (|Exterminator|_|) = patternFromEquality 55uy // The Exterminator
    let (|Shadow|_|) = patternFromEquality 56uy // The Shadow Queen
    let (|Greyskull|_|) = patternFromEquality 57uy // Greyskull the Pirate
    let (|Gloria|_|) = patternFromEquality 58uy // Gloria, the Good Witch
    let (|Ilsa|_|) = patternFromEquality 59uy // Ilsa, the Insane Wizard
    let (|Eiralon|_|) = patternFromEquality 60uy // Eiralon, the Blood Mage
    let (|PoT|_|) = patternFromEquality 61uy // Priestess of Time
    let (|Mindy|_|) = patternFromEquality 62uy // Mindy the Mime
    let (|HalfBlood|_|) = patternFromEquality 63uy // The Half-Blood Elf
    let (|Henry|_|) = patternFromEquality 64uy // Henry, the Scaredy-Ghoul
    let (|Fright|_|) = patternFromEquality 65uy // Fright-o-Tron 4000
    let (|Graham|_|) = patternFromEquality 66uy // Graham the Driver
    let (|Paul|_|) = patternFromEquality 67uy // Paul the Pilgrim
    let (|Petra|_|) = patternFromEquality 68uy // Petra the Pilgrim
    let (|DarkHelper|_|) = patternFromEquality 69uy // The Dark Helper
    let (|Robo|_|) = patternFromEquality 70uy // Robo-Rudolph
    let (|Bernard|_|) = patternFromEquality 71uy // Bernard the Bartender
    let (|Val|_|) = patternFromEquality 72uy // Princess Val the Mermaid
    let (|Karl|_|) = patternFromEquality 73uy // Karl the Kicker
    let (|Cindy|_|) = patternFromEquality 74uy // Cindy the Cheer-Orc
    let (|Warwick|_|) = patternFromEquality 75uy // Warwick the Warlock
    let (|Katie|_|) = patternFromEquality 76uy // Katie the Cupid
    let (|Snickette|_|) = patternFromEquality 77uy // Snickette the Sneaky
    let (|Squiggles|_|) = patternFromEquality 78uy // Squiggles the Clown
    let (|Arachnobuddy|_|) = patternFromEquality 79uy // Arachnobuddy
    let (|Foresight|_|) = patternFromEquality 80uy // Foresight
    let (|Baenarall|_|) = patternFromEquality 81uy // Baenarall, Angel of Hope
    let (|Sisaron|_|) = patternFromEquality 82uy // Sisaron the Dragon Sorceress
    let (|Spaceking|_|) = patternFromEquality 83uy // Spaceking
    let (|Grandmora|_|) = patternFromEquality 84uy // Grandmora
    let (|Devin|_|) = patternFromEquality 85uy // Devin the Dataminer
    let (|Darcy|_|) = patternFromEquality 86uy // Darcy the Dataminer
    let (|Danni|_|) = patternFromEquality 87uy // Danni the Daring Damsel
    let (|Polly|_|) = patternFromEquality 88uy // Polly the Parrot

open Crusaders

module Gearing = 
    type Rarity = 
        | Common
        | Uncommon
        | Rare
        | Epic of isGolden:bool
        | Legendary of isGolden:bool * level:byte
        
    let getAllDpsModifier= 
        function
        | Common -> 1.05m
        | Uncommon -> 1.10m
        | Rare -> 1.15m
        | Epic false -> 1.4m
        | Epic true -> 1.6m
        | Legendary (false,_) -> 1.8m
        | Legendary (true,_) -> 2.2m
        
    let getSelfDps =
        function 
        | Common -> 1.25m
        | Uncommon -> 1.5m
        | Rare -> 2m
        | Epic false -> 5m
        | Epic true -> 7m
        | Legendary (false,_) -> 9m
        | Legendary (true,_) -> 13m
    let getGold = 
        function
        | Common -> 1.1m
        | Uncommon -> 1.25m
        | Rare -> 1.5m
        | Epic false -> 2m
        | Epic true -> 2.5m
        | Legendary (false,_) -> 3m
        | Legendary (true,_) -> 4m
    let getLegendaryFactor level = 
        Math.Pow(2., float level - 1.)
    
    type GearedCrusader = {
        Crusader: Crusader 
        Slot0: Rarity option
        Slot1: Rarity option
        Slot2: Rarity option
    } with
        member x.AllGear() = 
            [x.Slot0;x.Slot1;x.Slot2]

open Gearing



module Layouts = 
    type Spot = |Spot of byte
    type Layout = Spot option list list
    type Row = | Row of byte
    type Column = | Column of byte
    type Position = | Position of Row * Column

    let (|Adjacent|_|) (p1,p2) = 
        match p1,p2 with
        | Position(Row r1, Column c1), Position(Row r2, Column c2) -> if abs(int r1 - int r2) <= 1 && abs(int c1 - int c2) <= 1 then Some () else None
    let (|InColumn|_|) ((r1,c1),(r2,c2)) = if r1 = r2 then Some () else None
    
    let getCrusaderSpot formation cruId = formation |> Seq.tryFindIndex ((=) cruId)
    let getDpsSpot formation dpsCruId = getCrusaderSpot formation dpsCruId
    let getHasAdjacentCru formation cruId = ()
    
open Layouts

type World = {
    	Name:string
        Id:byte
        // byte is slot number, option is because there can be gaps where no one is allowed
        Layout : Layouts.Layout} with
            member x.Slots 
                with get() = x.Layout |> Seq.map(Seq.choose id >> Seq.length) |> Seq.sum
            member x.GetRow spot =
                x.Layout |> Seq.tryFindIndex(fun row -> row |> Seq.choose id |> Seq.exists ((=) spot))
                |> Option.map (byte >> Row)
            member x.GetColumn spot = 
                x.Layout |> Seq.tryPick(fun row -> row |> Seq.tryFindIndex((=) (Some spot)))
                |> Option.map (byte >> Column)
            member x.GetPosition spot = 
                match x.GetRow spot, x.GetColumn spot with
                | Some r, Some c -> Some (Position (r,c))
                | Some _, None
                | None , Some _ -> failwithf "a row or column returned a value, buth not both for spot %A" spot
                | None, None -> None
                
            member x.GetPositions spot1 spot2 = 
                match x.GetPosition spot1, x.GetPosition spot2 with
                | Some s1, Some s2 -> Some(s1,s2)
                | None, None -> None
                | Some _, None -> failwithf "a spot wasn't in the layout %A" spot2 
                | None, Some _ -> failwithf "a spot wasn't in the layout %A" spot1
            
            // will this work for all cases? there are some columns that are semi-offset, yes?
            member x.IsAdjacent spot1 spot2 =
                match x.GetPositions spot1 spot2 with
                | Some Adjacent -> true
                | _ -> false
            member x.GetAdjacentSpots spot = 
                x.Layout
                |> Seq.concat
                |> Seq.choose id
                |> Seq.filter(x.IsAdjacent spot)
            member x.GetIsBehind front maybeBehind = 
                match x.GetPositions front maybeBehind with
                | Some (Position(_,Column c1),Position(_,Column c2)) when int c1 - int c2 = 1 -> true
                | _ -> false
            
// heroId list (position in list determines where in the layout that hero is)
                
let a = Some()
module Data = 
    type Source = |GameData | Data
    let source = GameData
    let toDump (x:JProperty) = (x.Name, x.Value)
    type JProperty with 
        member x.ToDump() =
            box (x.Name, x.Value)
    // should we ditch this and use the gamedata.json directly?
    let jsonData = 
        let relative =
            match source with
            | Data -> 
                let relative = @"data.js"
                relative
            | GameData ->
                let relative = @"gamedata.json"
                relative
            |> fun r -> Path.Combine(@"projects\CotLICheatSheet\js", r)
        let c = Path.Combine("C:\\", relative)
        let d = Path.Combine("D:\\", relative)
        if File.Exists d then d else c
        |> File.ReadAllText
        |> fun text ->
            match source with
            | Data ->
                text
                |> after "{" 
                |> (+) "{"
                |> before ";"
            | GameData ->
                text
        |> fun x-> JC.DeserializeObject<JObject>(x)
        
    let getCrusader (cruId:byte) = 
        let mapTags (t:JToken) = t :?> JArray |> Seq.cast<JValue> |> Seq.map (string>> Tag.FromRaw) |> List.ofSeq
        match source with
        | Data ->
            jsonData.["crusaders"] :?> JArray
            |> Seq.cast<JObject>
            |> Seq.find (fun c ->
                c.["id"] |> string |> byte = cruId
            )
            |> fun c -> {   Id= cruId
                            DisplayName= c.["displayName"] |> string
                            Tags= mapTags c.["tags"] 
                            Raw= box c
                            
            }
        | GameData ->
            jsonData.["hero_defines"] :?> JArray
            |> Seq.cast<JObject>
            |> Seq.find (fun c ->
                c.["id"] |> string = string cruId
            )
            |> fun c -> {   Id=cruId
                            DisplayName= c.["name"] |> string
                            Tags = c.["properties"].["tags"] |> mapTags
                            Raw = box c
                
            }
  
let worlds = 
    let ss = Spot >> Some
    Set[
        {   Name = "World's Wake"; Id = 1uy; 
            Layout = 
                [   [ss 0uy]
                    [ss  1uy; ss  5uy;]
                    [ss  2uy; ss  6uy; ss  9uy; ]
                    [ss  3uy; ss  7uy; ]
                    [ss  4uy; ss  8uy;]
        ]}
    ]
type HeroId = | HeroId of byte    
type Formation = HeroId option list
// able to account for doing dps calcs with or without gear
type Crusaderish = 
    | Cru of Crusader
    | Geared of GearedCrusader
    
type ModifierType = 
    | Ability
    | GearMod of fromLegendary:bool    
    | Positional
type ModifierEffect = 
    | Dps
    | Gold
    | Crit
       
type DpsCalculation =
    | IsDps
    | NotDps
 
let mutable dpsChar:Crusader option = None
// get dps modifiers of a crusader
let getDps includeWindUp (world:World,formation:Formation, dpsCru, dpsC:DpsCalculation) (c:Crusaderish) = 
    let getLegendaryLevel = 
        function
        | Legendary(_,level) ->
            Some level
        | _ -> None
    let getLegendarySlotLevel slot =
        match c with
        | Cru _ -> None
        | Geared gc -> 
            match slot with |0 -> gc.Slot0 |1 -> gc.Slot1 | 2 -> gc.Slot2 | x -> failwithf "unexpected slot %i" x
            |> Option.bind getLegendaryLevel
        
    let hId, tags, gearOpt = 
        match c with
        | Cru {Id=hId;Tags=tags} -> hId,tags, None
        | Geared gc -> 
            let gearOpt = 
                if [gc.Slot0; gc.Slot1; gc.Slot2] |> Seq.exists(Option.isSome) then
                    Some gc
                else None
            gc.Crusader.Id, gc.Crusader.Tags, gearOpt
    let zero = 1m
    match hId with
    | BushWhacker ->
        let result = 
            gearOpt
            |> Option.map(fun gc ->
                [   gc.Slot0 |> Option.bind getLegendaryLevel |> Option.map (fun l -> ModifierEffect.Crit, 1. * (getLegendaryFactor l))
                    gc.Slot1 |> Option.bind getLegendaryLevel |> Option.map (fun l -> ModifierEffect.Dps, 1. + 1. * (getLegendaryFactor l))
                ]
                |> List.choose id
                |> List.map(fun (e,ef) -> GearMod true, e,ef)
            )
            |> Option.getOrDefault List.empty
        result
    | Jim ->
        [
            match dpsC with
            | IsDps ->
                // buddy system
                // if there is at least 1 adjacent, he gets a self-buff
                let hasAdjacentCrusader = 
                    let spot = formation |> Seq.findIndex(fun hIdOpt -> hIdOpt = Some (HeroId hId)) |> byte
                    Spot spot
                    |> world.GetAdjacentSpots 
                    |> Seq.exists(fun (Spot adjSpot) -> Option.isSome formation.[int adjSpot])
                if hasAdjacentCrusader then
                    match getLegendarySlotLevel 1 with
                    | None -> yield Some (Ability,ModifierEffect.Dps,1.5)
                    | Some level -> yield Some (GearMod true, Dps, 1. + getLegendaryFactor level)
                    | _ -> ()
    
            | NotDps -> ()
                // column buff
//                if world.GetColumn dpsSpot = 
//            
//                [   Positional, Dps, 1.5
//                ] 
        ]
        |> List.choose id
    | _ -> List.empty
  

