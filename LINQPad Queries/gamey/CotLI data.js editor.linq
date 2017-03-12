<Query Kind="FSharpProgram">
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

// desired: fix these to use upper case, but serialize back to lower

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
let path = @"D:\Projects\CotLICheatSheet\js\data.js"
let flip f y x = f x y
let dumpt (t:string) x = x.Dump(t,exclude="Raw"); x
let dumpRemoval (items:string seq) x = 
    let x = Util.ToExpando x
    let dic = (box x) :?> IDictionary<string,obj>
    items
    |> Seq.iter(dic.Remove >> ignore<bool>)
//    x.Dump("duplicateDump")
    x
let dumpRemoveRaw x = 
    x |> dumpRemoval ["Raw"]
let getStr (name:string) (jo:JObject) = jo.[name] |> string
let getPropertyNames (jo:JObject) = jo.Properties() |> Seq.map (fun p -> p.Name) |> List.ofSeq
let hasProperty (name:string) (jo:JObject) = jo.Property(name) |> isNull |> not
let setProperty (name:string) (value:obj) jo = 
    let t = JToken.FromObject value
    if jo |> hasProperty name then 
        // would be nice if this took objects and camelcased them
        jo.Property(name).Value <- t
    else
        printfn "adding new property %s" name
        jo.Add(name,t)
type MissionTag={Id:string; DisplayName:string; Image:string; Raw:JObject} with member x.ToDump() = x |> dumpRemoveRaw

type CrusaderLoot = {LootId:int; Name:string; Rarity:int; Slot:int option}        
type Crusader= { Id:string;Link:string;DisplayName:string; HeroId:string; Loot: CrusaderLoot list; Raw:JObject} with member x.ToDump() = x |> dumpRemoveRaw

type CrusaderData = {Wikibase: string; MissionTags: MissionTag list; Crusaders: Crusader list; Raw:JObject} with member x.ToDump() = x |> dumpRemoveRaw
        
let mapMissionTag (jo:JObject) = 
    {Id = jo.["id"] |> string; DisplayName= jo.["displayName"] |> string; Image= jo.["image"] |> string; Raw=jo}
let mutable succeededOnce = false    
let mapCrusader (jo:JObject) = 
    {   Id = jo.["id"] |> string
        DisplayName= jo.["displayName"] |> string
        Link = getStr "link" jo
        Loot = 
//            let ja = jo.["loot"] :?> JArray |> Option.ofObj |> Option.map Seq.cast<JObject> |> Option.getOrDefault Seq.empty |> List.ofSeq
//            
//            if not<| List.isEmpty ja then
//                ja |> List.map (fun jo -> 
//                    try
//                        let lootId = jo.Property("id").Value |> Dump |> int
//                        printfn "finished id prop"
//                        let result ={LootId= lootId; Name=jo.Property("Name").Value |> string; Rarity= jo.Property("Rarity").Value |> int}
//                        succeededOnce <- true
//                        result
//                    with ex ->
//                        (getPropertyNames jo,jo,succeededOnce).Dump("failed to map loot")
//                        reraise()
//                    )
//                
////                (ja.Properties() |> Seq.map (fun jp -> jp.Name) |> List.ofSeq,ja).Dump("found items, but didn't read them in")
//            else []
            []
        Raw = jo
        HeroId= jo.["heroId"] |> string
        }
let (|StartsWithI|_|) (d:string) (s:string) = 
    if s.StartsWith(d, StringComparison.InvariantCultureIgnoreCase) then Some () else None
let mutable hasChanges = false
module MappedChanges = 
    let (|IsInt|_|) (x:string) = match Int32.TryParse x with | true, x -> Some x |_ -> None
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
    type LootItem = {Id:int; HeroId:int; Rarity:int; Name:string}
    let addLootItems (x:CrusaderData) = 
        let lootItems = 
            File.ReadAllLines @"D:\Projects\CotLICheatSheet\js\lootdata.json" 
            |> Seq.filter(fun s -> not <| isNull s && s.Contains ";")
            |> Seq.map (fun s-> s.Split(Array.singleton ';'))
            |> Seq.map (function
                | [| lootId;heroId;rarityId;name |] ->
                    {Id=int lootId; HeroId=int heroId; Rarity=int rarityId; Name=name}
                | x -> 
                    x.Dump("failing, unexpected loot array")
                    raise <| InvalidOperationException()
            )
            |> List.ofSeq
        x.Crusaders
        |> List.map (fun cru ->
            match cru.Loot with
            // overwrite always, while we work on this stuff
            | _ ->
                let loot = 
                    lootItems 
                    |> Seq.filter(fun li -> li.HeroId = int cru.HeroId) 
                    |> Seq.map (fun li -> {  LootId=li.Id; 
                                                Rarity=li.Rarity; Name=li.Name; 
                                                Slot= None})
                                                
                                                
                    // if we could keep track of the previous item, we'd be better off reading a lot until the rarity value went back down. as they are already sorted by slot in the input
                    |> Seq.sortBy (fun li -> li.Rarity, li.LootId)
                    |> Seq.mapi(fun i li -> // can't map slots greater than 8 (without count of greater epics that exist)
                        if i < 9 && li.Rarity < 5 then 
                            {li with Slot = Some <| i % 3} 
                        else li
                    )
                    |> List.ofSeq
                let lootRaw = 
                    loot
                    |> Seq.map (fun x ->
                        let result = JObject()
                        result |> setProperty "lootId" x.LootId
                        result |> setProperty "rarity" x.Rarity
                        match x.Slot with
                        | Some slot -> 
                            result |> setProperty "slot" slot
                        | None -> ()
                        // can't do this without slot information
                        if loot |> Seq.exists (fun l -> l.LootId < x.LootId && l.Slot = x.Slot && x.Rarity = l.Rarity ) then
                            result |> setProperty "isGreater" true
                        result |> setProperty "name" x.Name
                        result
                    )
                    |> List.ofSeq
                cru.Raw |> setProperty "loot" lootRaw
                hasChanges <- true
                (cru.DisplayName,loot).Dump()
                {cru with Loot = loot}
                
            //| _ -> cru
            cru
        )
        |> fun y -> {x with Crusaders = y}
    let addOrSetHeroIds (x:CrusaderData) =
        let mutable abort = false
        x.Crusaders
        |> List.map (fun cru ->
            if not abort && String.IsNullOrWhiteSpace cru.HeroId then
                cru.Id 
                |> slotMap
                |> fun i -> if i < 1 then None else Some i
                |> function
                    | Some heroId -> 
                        cru.Raw |> setProperty "heroId" heroId
                        hasChanges <- true
                        {cru with HeroId=heroId |> string}
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
let starter,text,trailer = 
    let text = File.ReadAllText(path)
    text |> before "=", text |> after "=" |> before ";", text |> after ";"
(starter,trailer).Dump("starter,trailer")    
text
|> trim
|> fun x -> Regex.Replace(x,"\"(\w+)\"\s*:","$1: ")
|> hoist dumpReverse
|> fun x -> Newtonsoft.Json.JsonConvert.DeserializeObject<JObject> x
|> hoist dumpReverse
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
                    |> Seq.map mapCrusader
                    |> List.ofSeq
                Raw = x}
// what change operation?
//|> MappedChanges.setEmptyLinks
//|> MappedChanges.addOrSetHeroIds
|> MappedChanges.addLootItems
|> hoist dumpReverse
|> fun x -> Newtonsoft.Json.JsonConvert.SerializeObject(x.Raw, Newtonsoft.Json.Formatting.Indented)
|> fun x -> sprintf "%s=\r\n%s%s;" starter x trailer
|> fun x -> File.WriteAllText(path, x)
//|> dumpReverse