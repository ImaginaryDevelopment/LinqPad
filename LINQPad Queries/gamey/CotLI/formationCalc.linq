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
            |"clicker" -> Clicker
            | "elf" -> Elf
            | "orc" -> Orc
            
        
    type Crusader = 
        {   DisplayName:string; 
            // abandoning/not implementing the 01a nomenclature or .. deriving it
            Id: byte
            Tags: Tag list
            Raw:obj}
        member private x.ToDump() = 
            let y = {x with Raw = x.Raw |> string}
            y
    
//    let (|BushWhacker|_|) = fromByte 1uy
//    match 1uy with
//    | Bushwhacker -> printfn "found bush!"
//    
//    let (|RoboRabbit|_|) = fromByte 38uy
//    let (|Graham|_|) = fromByte 66uy
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
        Slot0: Rarity
        Slot1: Rarity
        Slot2: Rarity
    }

open Gearing

// able to account for doing dps calcs with or without gear
type Crusaderish = 
    | Cru of Crusader
    | Geared of GearedCrusader
    
type ModifierType = 
    | Ability
    | GearMod
    | Legendary
    
type CrusaderDps = 
    {   GlobalDpsModifier: (decimal * ModifierType) list
        GlobalGoldModifier: (decimal * ModifierType) list
        CritChance: byte
    }
    static member Zero = 
        {   GlobalDpsModifier= List.empty
            GlobalGoldModifier= List.empty
            CritChance= 0uy
        }        
type Positioning = | SameRow | InFront|Behind|Adjacent | Other
type DpsCalculation =
    | IsDps
    | NotDps of Positioning
let getDps dpsC includeWindUp cSpot c = 
    let cruId, tags = 
        match c with
        | Cru {Id=cruId;Tags=tags} -> cruId,tags
        | Geared {Crusader = {Id=cruId;Tags=tags} } -> cruId, tags
    let zero = 1m
    match cruId,dpsC with
    | 1uy, _ -> zero
    | 2uy, NotDps SameRow -> // jim
        1.5m
    | 2uy, _ -> zero
    |>
        // insure all 0 multipliers become 1 as no ability shuts off all dps (cept perhaps draco on himself?)
        function
        |0m -> 1m
        | x -> x
    ()

        



let mutable dpsChar:Crusader option = None

module Layouts = 
    type Layout = byte option list list
    type Spot = |Spot of byte
    type Row = | Row of byte
    type Column = | Column of byte
    type Position = | Position of Row * Column

    let (|Adjacent|_|) (p1,p2) = 
        match p1,p2 with
        | Position(Row r1, Column c1), Position(Row r2, Column c2) -> if abs(int r1 - int r2) <= 1 && abs(int c1 - int c2) <= 1 then Some () else None
    let (|InColumn|_|) ((r1,c1),(r2,c2)) = if r1 = r2 then Some () else None
    
    let getCrusaderSpot formation cruId = formation |> Seq.tryFindIndex ((=) cruId)
    let getDpsSpot formation dpsCruId = getCrusaderSpot formation dpsCruId
open Layouts  

type World = {
    	Name:string
        Id:byte
        // byte is slot number, option is because there can be gaps where no one is allowed
        Layout : Layouts.Layout} with
            member x.Slots 
                with get() = x.Layout |> Seq.map(Seq.choose id >> Seq.length) |> Seq.sum
            member x.GetRow y = 
                match y with
                | Spot spot ->
                    x.Layout |> Seq.tryFindIndex(fun row -> row |> Seq.choose id |> Seq.exists ((=) spot))
                    |> Option.map (byte >> Row)
            member x.GetColumn y = 
                match y with
                | Spot spot ->
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
                
            member x.GetIsBehind front maybeBehind = 
                match x.GetPositions front maybeBehind with
                | Some (Position(_,Column c1),Position(_,Column c2)) when int c1 - int c2 = 1 -> true
                | _ -> false
            
// heroId list (position in list determines where in the layout that hero is)
type Formation = byte option list                
let a = Some()
module Data = 
    type Source = |GameData | Data
    let source = GameData
    let toDump (x:JProperty) = 
        (x.Name, x.Value)
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
    Set[
        {   Name = "World's Wake"; Id = 1uy; 
            Layout = 
                [   [Some 0uy]
                    [Some 1uy; Some 5uy;]
                    [Some 2uy; Some 6uy; Some 9uy; ]
                    [Some 3uy; Some 7uy; ]
                    [Some 4uy; Some 8uy;]
        ]}
    ]



worlds
|> Dump
|> ignore
Data.getCrusader 1uy
|> Dump
|> ignore