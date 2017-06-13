<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

let cotLIPath = if Directory.Exists(@"D:\projects\CotLICheatSheet\") then @"D:\projects\CotLICheatSheet\js" else @"C:\projects\CotLICheatSheet\js"
[<AutoOpen>]
module Helpers =
    let hoist f x = f x; x
    let dumpReverse :  (obj) -> unit =
        let dc = DumpContainer()
        dc.Dump() |> ignore
        (fun o -> 
            match dc.Content with
            | :? (obj list) as items -> List.Cons(o,items)
            | _ -> [ o ]
            |> fun content -> dc.Content <- content
        )
        
    let (|IsInt|_|) (x:string) = match Int32.TryParse x with | true, x -> Some x |_ -> None
    let flip f y x = f x y
    let inline dumpt (t:string) x = x.Dump(t,exclude="Raw") |> ignore; x
    let dumpRemoval (items:string seq) x = 
        let x = Util.ToExpando x
        let dic = (box x) :?> IDictionary<string,obj>
        items
        |> Seq.iter(dic.Remove >> ignore<bool>)
    //    x.Dump("duplicateDump")
        x
    let dumpRemoveRaw x = 
        x |> dumpRemoval ["Raw"]
        
        
[<AutoOpen>]
module JsonHelpers =
    let getStr (name:string) (jo:JObject) = jo.[name] |> string
    let getPropertyNames (jo:JObject) = jo.Properties() |> Seq.map (fun p -> p.Name) |> List.ofSeq
    let getProperty (name:string) (jo:JObject) : JProperty option = jo.Property name |> Option.ofObj
    let getPropertyValue name jo = getProperty name jo |> Option.map (fun jp -> jp.Value) |> Option.bind Option.ofObj
    let getArray name jo = getProperty name jo |> Option.map (fun jp -> jp.Value |> fun x -> x :?> JArray)
    let getArrayT<'T> name jo = getArray name jo |> Option.map (Seq.cast<'T>)
//    let getObjectArray name jo = getArray name jo |> Option.map (Seq.cast<JObject>)
//    let getValueArray name jo = getProperty name jo |> Option.map (fun jp -> jp.Value |> fun x -> x :?> JArray |> Seq.cast<JObject>)
    let deserializeJO x = Newtonsoft.Json.JsonConvert.DeserializeObject<JObject>(x)
    let hasProperty (name:string) (jo:JObject) = jo.Property(name) |> isNull |> not
    let setProperty (name:string) (value:obj) jo = 
        let t = JToken.FromObject value
        if jo |> hasProperty name then 
            // would be nice if this took objects and camelcased them
            jo.Property(name).Value <- t
        else
            //printfn "adding new property %s" name
            jo.Add(name,t)
            
            
            
type MissionTag={Id:string; DisplayName:string; Image:string; Raw:JObject} with member x.ToDump() = x |> dumpRemoveRaw

type CrusaderLoot = {Slot:int; Rarity:int; IsGolden:bool; Name:string; LootId:int} with 
    static member toJObject(x:CrusaderLoot) = 
        
        let result = JObject(JProperty("slot",x.Slot - 1),JProperty("rarity",x.Rarity), JProperty("name",x.Name), JProperty("lootId",x.LootId))
        let jGolden = if x.IsGolden then JProperty("golden",true) else null
        if x.IsGolden then
            result.Add jGolden
        result


let (|StartsWithI|_|) (d:string) (s:string) = 
    if s.StartsWith(d, StringComparison.InvariantCultureIgnoreCase) then Some () else None
let mutable hasChanges = false

module MyDataFile = 
    type Crusader= { Id:string;Link:string;DisplayName:string; HeroId:int; BenchSlot:int; Loot: CrusaderLoot list; Raw:JObject; Gear: string list } with member x.ToDump() = x |> dumpRemoveRaw

    type CrusaderData = {Wikibase: string; MissionTags: MissionTag list; Crusaders: Crusader list; Raw:JObject} with member x.ToDump() = x |> dumpRemoveRaw
    let path = Path.Combine(cotLIPath,"data.js")
            
    let mapMissionTag (jo:JObject) = 
        {Id = jo.["id"] |> string; DisplayName= jo.["displayName"] |> string; Image= jo.["image"] |> string; Raw=jo}
    
    
    let mapCrusader (jo:JObject) = 
        {   Id = jo.["id"] |> string
            BenchSlot= jo.["id"] |> string |> (fun id -> id.[0..1]) |> int
            DisplayName= jo.["displayName"] |> string
            Link = getStr "link" jo
            Loot = []
            Raw = jo
            HeroId= jo.["heroId"] |> string |> int
            Gear = jo |> getArrayT<JValue> "gear" |> Option.getOrDefault Seq.empty |> Seq.map (fun x -> x.Value) |> Seq.cast<string> |> List.ofSeq
            }
            
            
module GameData = 
    type LootItem = {HeroId:int; CL:CrusaderLoot}
    let gameData = lazy(
        let x = 
            Path.Combine(cotLIPath,"gamedata.json")
            |> File.ReadAllText
            |> deserializeJO
        let pv = 
            x 
            |> getProperty "patch_version"
        pv
        |> dumpt "patch_version"
        |> ignore
        x
    )
    let lootItems = lazy(
            let x = 
                gameData.Value
            
            let getInt name (jo:JObject) = 
                jo |> getPropertyValue name |> Option.get |> string |> int
            x 
            |> getPropertyValue "loot_defines"
            |> Option.get
            |> fun x -> x :?> JArray
            |> Seq.cast<JObject>
            |> Seq.map (fun raw ->
                {   HeroId= raw |> getInt  "hero_id"
                    CL ={   LootId=raw |> getInt "id"
                            Rarity=raw |> getInt "rarity"
                            Name= raw |> getPropertyValue "name" |> Option.get |> string
                            Slot= raw |> getInt "slot_id"
                            IsGolden= 
                                raw 
                                |> getPropertyValue "golden" 
                                |> Option.get 
                                |> string 
                                |> function 
                                    |"0" -> false 
                                    |"1" -> true 
                                    | x -> 
                                        x.Dump("Invalid option for field golden")
                                        raise <| InvalidOperationException(message="Invalid option for field golden")
                    }
                }
            )
         
            |> Seq.filter(fun li -> li.HeroId > 0)
            |> Seq.groupBy(fun li -> li.HeroId)
            |> Seq.map (fun (heroId,items) -> 
                (heroId, 
                    items
                    |> Seq.map(fun li -> li.CL) 
                    |> Seq.sortBy(fun cl -> cl.Slot, cl.Rarity, cl.LootId) 
                    |> List.ofSeq
                )
            )
            |> Map.ofSeq
    )
    type Hero = {Id:int;Name:string;SeatId:int;Tags:string list; Raw:obj}
    let heroes = lazy(
        let x = gameData.Value
        x
        |> getPropertyValue "hero_defines"
        |> Option.get
        |> fun x -> x :?> JArray
        |> Seq.cast<JObject>
        |> Seq.map(fun raw ->
            //raw |> string |> dumpt"hero!" |> ignore
            let propsNode = raw |> getPropertyValue "properties" |> Option.get
            {   Id=raw |> getPropertyValue "id" |> Option.get |> string |> int
                Name=raw |> getPropertyValue "name" |> Option.get |> string
                SeatId= raw |> getPropertyValue "seat_id" |> Option.get |> string |> int
                Tags = 
                    propsNode :?> JObject
                    |> getPropertyValue "tags" 
                    |> Option.get 
                    |> fun x -> x :?> JArray 
                    |> Seq.cast<JValue> 
                    |> Seq.map(fun rt -> rt.Value |> string) |> List.ofSeq
                    
                Raw=raw
                }
        )
        //|> List.ofSeq
    )
    
    
module MappedChanges = 
    open MyDataFile
    
    let slotMap= 
        function
        |IsInt x when x < 21 -> x
        | "01a" -> 38
        | "01b" -> 66
        | "01c" -> 75
        |"02a" -> 29
        | "02b" -> 51
        // Sally
        | "03a" ->34
        // Karen
        |"03b" -> 53
        | "04a" -> 31
        | "04b" -> 62
        | "05a" -> 36
        // Draco
        |"05b" -> 46
        // Henry
        |"05c" -> 64
        
        // Mister
        | "06a" -> 21
        // Larry
        | "06b" -> 35
        |"06c" -> 71 // Bernard
        |"07a" -> 25 // RoboTurkey
        |"07b" -> 49 // Rayna
        |"08a" -> 24
        |"08b" -> 47 // President Billy
        |"08c" -> 73 // Karl
        |"09a" -> 22 // Pete
        |"09b" -> 41 // Broot
        |"09c" -> 67
        |"10a" -> 32
        |"10b" -> 52
        |"11a" -> 26 // Momma Kaine
        |"11b" -> 44 // Brogon
        |"11c" -> 63 // The Half-blood elf
        |"12a" -> 33
        |"12b" -> 45
        |"12c" -> 69
        |"13a" -> 40 // The Metal Soldierette
        |"13b" -> 77 // Snickette
        |"14a" -> 27 // RoboSanta
        |"14b" -> 43 // Leerion
        |"14c" -> 76 // Katie
        |"15a" -> 23 // Wendy
        |"15b" -> 42 // Robbie
        |"15c" -> 72
        |"16a" -> 37 // Alan
        |"16b" -> 65 // Fright
        |"17a" -> 30 // Queen Siri
        // Boggins
        |"17b" -> 54
        |"17c" -> 78
        // Frosty
        |"18a" -> 28
        //Littlefoot
        |"18b" -> 50
        |"18c" -> 74 // Cindy
        |"19a" -> 39 // Bat
        |"19b" -> 68 // Petra
        |"20a" -> 48 //Kiz
        |"20b" -> 70 // Robo
        // Exterminator
        |"21" -> 55
        // Gloria
        |"21a" -> 58
        |"22" -> 56
        |"22a" -> 59
        |"23" -> 57
        |"23a" -> 60
        |"24" -> 61
        | x -> 
            printfn "found nothing for slotMap %A" x
            0
    // type CrusaderLoot = {LootId:int; Name:string; Rarity:int; Slot:int option; Golden:bool}
    
    // take the current loot information from gamedata.json and import it
    let addLootItemsFromGameData (x:CrusaderData) = 
        let lootItems = GameData.lootItems.Value

        x.Crusaders
        |> Seq.map (fun c ->
            hasChanges <- true
            c.Raw |> setProperty "loot" (lootItems.[int c.HeroId] |> Seq.sortBy (fun l -> l.Slot,l.Rarity,l.IsGolden) |> Seq.map CrusaderLoot.toJObject)
            {c with Loot =lootItems.[int c.HeroId]}
        )
        |> List.ofSeq
        |> fun y -> {x with Crusaders = y}

    // uses the lookup above (slotMap) to set the HeroId (1..x) int value
    let addOrSetHeroIds (x:CrusaderData) =
        let mutable abort = false
        x.Crusaders
        |> List.map (fun cru ->
            if not abort && cru.HeroId < 1 then
                cru.Id 
                |> slotMap
                |> fun i -> if i < 1 then None else Some i
                |> function
                    | Some heroId -> 
                        cru.Raw |> setProperty "heroId" heroId
                        hasChanges <- true
                        {cru with HeroId=heroId |> string |> int}
                    | None -> cru
            else
                cru
        )
        |> fun y -> {x with Crusaders = y}
            
    let setEmptyLinks (x:CrusaderData) =    
        let cleanInput =
            function
            | null -> null
            | "" -> null
            | (StartsWithI x.Wikibase) as l -> l |> after x.Wikibase
            | l -> l
        let crusaders = 
            let mutable abort = false
            x.Crusaders
            |> List.map (fun cru -> 
                        if String.IsNullOrWhiteSpace cru.Link && not abort then 
                            let link = Util.ReadLine(sprintf "link for %s?" cru.DisplayName)
                            if String.IsNullOrWhiteSpace link then
                                abort <- true
                                cru
                            else
                                let link = link |> cleanInput
                                cru.Raw |> setProperty "link" link
                                hasChanges <- true
                                {cru with Link = link } 
                        else cru
            )
        
        {x with Crusaders=crusaders}
        
    // crusaders that don't have the "gear": [ "alldps,"gold","selfdps"] property, or don't have values in it
    let findCrusadersMissingGearData (x:CrusaderData) = 
        let gearTypes = 
            x.Crusaders
            |> Seq.map (fun c -> c.Gear |> Seq.ofList ) 
            |> Seq.concat 
            |> Seq.distinct 
            |> List.ofSeq
        gearTypes.Dump("gear types")
        
        let gd = GameData.gameData.Value

        let loot = gd |> getArrayT<JObject> "loot_defines"
        let mutable saveAndQuit = false
        {x with 
            Crusaders = 
                x.Crusaders
                |> Seq.map (fun cru ->
                    if saveAndQuit || cru.Gear.Length <> 0 then
                        cru
                    else
                        let mutable skip = false
                        [0..2]
                        |> Seq.map (fun i -> 
                            if saveAndQuit then
                                null 
                            else 
                                match if skip then "skip" else Util.ReadLine(sprintf "gear for %s slot %i?" cru.DisplayName i,"alldps", gearTypes |> List.filter(fun a -> a.StartsWith("ability") |> not)) with
                                | null | "" ->
                                    saveAndQuit <- true
                                    null
                                | "skip" ->
                                    skip <- true
                                    null
                                | x -> x
                        )
                        |> List.ofSeq
                        |> function
                            | _ when saveAndQuit = true -> cru
                            | x when x |> Seq.exists(isNull) -> 
                                cru
                            | x when x |> Seq.exists(fun x-> x = "skip") -> cru
                            | [a;b;c] as gears -> 
                                let arr = Array.ofList gears
                                cru.Raw |> setProperty "gear" arr
                                cru.Raw.Property("gear").Value.Dump("changed it?")
                                hasChanges <- true
                                {cru with Gear = gears}
                            | _ -> 
                                // something was entered wrongish
                                saveAndQuit <-true
                                cru
                            
                        
                )
                |> List.ofSeq
        }

    let addMissingCrusaders (x:CrusaderData) = 
        let makeIdentifier (seatId:int) = 
            function
            | 0 -> seatId |> string
            | x -> 
                x - 1
                |> char 
                |> (+) 'a'
                |> string
                |> (+) (seatId |> string |> fun x -> x.PadLeft(2,'0'))

        // heroes (id,name,seat,tags) are mapped, need to find ones that are missing and add them to x next
        GameData.heroes.Value
        
        |> Seq.filter(fun h -> x.Crusaders |> Seq.exists(fun cru -> cru.HeroId = h.Id) |> not)
        |> List.ofSeq
        |> function
            | [] -> x
            | missingHeroes ->
                //x.Raw.["crusaders"] :?> JArray
                // change x.Crusaders, x.Raw.["crusaders"] :?> JArray, and hasChanges
                hasChanges <- true
                x.Raw.["crusaders"] :?> JArray
                // normally would be id,displayName,image,slot, ...
                |> fun cruArray -> 
                    missingHeroes
                    |> Seq.map(fun hero ->
                       
                        let identifier = 
                            x.Crusaders 
                            |> Seq.filter(fun cru -> cru.BenchSlot = hero.SeatId) 
                            |> Seq.map(fun cru -> cru.HeroId) 
                            |> Seq.append [hero.Id] 
                            |> Seq.sortBy id
                            |> Seq.findIndex((=) hero.Id)
                            |> makeIdentifier hero.SeatId
                        let cruRaw = 
                            JObject(
                                // TODO: event, eventLink, loot (reference loot data), gear (gear is what each loot slot does for the person if present)
                                JProperty("id",identifier),
                                JProperty("heroId",hero.Id),
                                JProperty("displayName",hero.Name),
                                JProperty("image",String.Empty),
                                JProperty("slot",hero.SeatId),
                                JProperty("tags",JArray(hero.Tags |> List.map JValue |> Array.ofList))
                            )
                        (cruRaw |> string,cruRaw).Dump("cruRaw")
                        // prior should be the one in the same seat but cru.heroId < hero.Id or last
                        let priorCru = 
                            x.Crusaders 
                            |> Seq.filter(fun cru -> cru.BenchSlot = hero.SeatId) 
                            |> List.ofSeq 
                            |> List.rev 
                            |> List.tryHead 
                        match priorCru with
                        | Some cru -> 
                            let targetIndex = x.Crusaders |> List.findIndex((=) cru)                            
                            cruArray.Insert(targetIndex, cruRaw)
                        | None -> cruArray.Add cruRaw
                        cruArray |> string |> dumpt "inserted!" |> ignore
                        {Id=identifier;HeroId=hero.Id;DisplayName=hero.Name;BenchSlot=hero.SeatId;Link=String.Empty;Raw=cruRaw;Gear=List.empty;Loot=List.empty}
                    )
                    |> fun crus -> 
                        {x with Crusaders = 
                                crus 
                                |> Seq.append x.Crusaders 
                                |> Seq.sortBy(fun cru -> cru.BenchSlot,cru.HeroId) 
                                |> List.ofSeq }

open MyDataFile
open GameData
let starter,text,trailer = 
    let text = File.ReadAllText(MyDataFile.path)
    text |> before "=", text |> after "=" |> before ";", text |> after ";"
text
|> trim
|> fun x -> Regex.Replace(x,"\"(\w+)\"\s*:","$1: ")
//|> hoist dumpReverse
|> fun x -> Newtonsoft.Json.JsonConvert.DeserializeObject<JObject> x
//|> hoist dumpReverse
|> fun x -> {
                Wikibase = x.["wikibase"] |> string
                MissionTags= 
                    x.["missionTags"] :?> JArray 
                    |> Seq.cast<JObject> 
                    |> Seq.map mapMissionTag 
                    |> List.ofSeq
                Crusaders=
                    x.["crusaders"] :?> JArray
                    |> Seq.cast<JObject>
                    |> Seq.map (fun cru -> 
                        try
                            mapCrusader cru
                        with ex ->
                            (cru |> string,cru).Dump("failed")
                            reraise()
                    )
                    |> List.ofSeq
                Raw = x}
// what change operation?
//|> MappedChanges.setEmptyLinks
//|> MappedChanges.addOrSetHeroIds
//|> MappedChanges.findCrusadersMissingGearData
//|> MappedChanges.addMissingCrusaders
|> MappedChanges.addLootItemsFromGameData
|> hoist dumpReverse
|> fun x -> Newtonsoft.Json.JsonConvert.SerializeObject(x.Raw, Newtonsoft.Json.Formatting.Indented)
|> fun x -> sprintf "%s=\r\n%s%s;" starter x trailer
|> fun x -> if hasChanges then File.WriteAllText(path, x) else ()