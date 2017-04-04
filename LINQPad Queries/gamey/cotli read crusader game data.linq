<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

open Newtonsoft.Json
open Newtonsoft.Json.Linq
// WIP
//type PascalPropertyNamesContractResolver() =
//    inherit Newtonsoft.Json.Serialization.DefaultContractResolver()
//    static let distinctResolutionNames = HashSet<string>()
//    static member GetResolutionNames () = distinctResolutionNames :> seq<_>
//    override x.ResolvePropertyName s =
//        let result = Regex.Replace(s, @"_(\w)","\U\1")
//        distinctResolutionNames.Add s |> ignore
//        //printfn "resolved %s to %s" s result
//        result
let dumpRemoval (items:string seq) x = 
    let x = Util.ToExpando x
    let dic = (box x) :?> IDictionary<string,obj>
    items
    |> Seq.iter(dic.Remove >> ignore<bool>)
    x
let dumpRemoveRaw x = 
    x |> dumpRemoval ["Raw"]
    
let dumpIsNull title x = 
    if isNull <| box x then
        title.Dump("isNull")
    x    
let serializerSettings = JsonSerializerSettings(ContractResolver=Serialization.DefaultContractResolver(NamingStrategy=Serialization.SnakeCaseNamingStrategy()))
let serializeRaw x = Newtonsoft.Json.JsonConvert.SerializeObject x
let serialize x = Newtonsoft.Json.JsonConvert.SerializeObject(x, Newtonsoft.Json.Formatting.Indented)        
let deserialize<'T> x: 'T = Newtonsoft.Json.JsonConvert.DeserializeObject<'T>(x, serializerSettings)
let getProperty name (x:JObject) = x.Property name |> Option.ofObj
let getPropertyValue (x:JProperty) = x.Value |> Option.ofObj
let getPropertyValueOrFail name =
    getProperty name
    >> dumpIsNull (sprintf "property %s" name)
    >> Option.get
    >> dumpIsNull (sprintf "value %s" name)
    >> getPropertyValue
    >> dumpIsNull (sprintf "value2 %s" name)
    >> Option.get
    
    
let asJObject (x:obj) = x :?> JObject
let asJArray (x:obj) = x :?> JArray
let hoist f x =
    f x |> ignore
    x

let data = deserialize<JObject>  (File.ReadAllText @"C:\projects\cotlicheatsheet\js\gamedata.json")
type CrusaderData = {SeatId:int; HeroId:int;Name:string; Tags: string list; Raw:obj}
let heroData = 
    data
    |> getPropertyValueOrFail "hero_defines" 
    |> asJArray
    |> dumpIsNull "hero_defines"
    |> Seq.cast<JObject>
    |> Seq.map (fun jo -> {
                            SeatId= jo |> getPropertyValueOrFail "seat_id" |> int
                            HeroId= jo |> getPropertyValueOrFail "id" |> int
                            Name= jo |> getPropertyValueOrFail "name" |> string
                            Tags= jo  |> getPropertyValueOrFail "properties" |> asJObject |> getPropertyValueOrFail "tags" |> asJArray |> Seq.map string |> List.ofSeq
                            Raw = jo
                            }
    )
    |> List.ofSeq
type Rarity = 
    |Common
    |Uncommon
    |Rare
    |Epic
    |Legendary
    
type LootData = {LootId:int; Name:string; (* Description:string; *)  Rarity:Rarity}
type HeroGear = {LootData:LootData; IsGolden:bool; HeroId:int; SlotId:int}
type Loot = 
    |HeroGear of HeroGear
    |Other of LootData
let lootData = 
    data.Property("loot_defines").Value :?> JArray
    |> Seq.cast<JObject>
    |> Seq.map (fun jo ->
        let lootData = {    LootId= jo |> getPropertyValueOrFail "id" |> int
                            Name= jo |> getPropertyValueOrFail "name" |> string
                            Rarity= jo |> getPropertyValueOrFail "rarity" |> int |> function | 1 -> Common | 2 -> Uncommon | 3 -> Rare | 4 -> Epic |5 -> Legendary}
//        SlotId = jo |> getPropertyValueOrFail "slot_id" |> int |> function | 0 -> None | x -> Some x
//        HeroId = jo |> getPropertyValueOrFail "hero_id" |> int |> function | 0 -> None | x -> Some x}
        match jo |> getPropertyValueOrFail "hero_id" |> int with 
        | 0 -> Other lootData
        | x -> HeroGear {   LootData=lootData
                            IsGolden= jo |> getPropertyValueOrFail "golden" |> int |> (=) 1
                            SlotId = jo |> getPropertyValueOrFail "slot_id" |> int
                            HeroId = x}
    )
    |> List.ofSeq
    
let getHeroInfo () =     
    let seatId = Util.ReadLine("seatId?", 18, heroData |> Seq.map(fun h -> h.SeatId))

    let heroId,hero,seatMates = 
        let seated = heroData |> Seq.filter(fun h -> (h.SeatId ) = seatId) |> List.ofSeq
        seated |> Seq.map dumpRemoveRaw |> fun x -> x.Dump("heroes in seat")
        let heroId = Util.ReadLine("heroId?", 15, seated |> Seq.map (fun h -> h.HeroId))
//        seated |> Seq.map dumpRemoveRaw |> Dump
        heroId, seated |> Seq.find(fun h -> h.HeroId = heroId), seated
    hero, seatMates
let getHeroLoot heroId = 
    lootData
    |> Seq.choose(function 
                    | HeroGear hg -> if hg.HeroId = heroId then Some hg else None 
                    | _ -> None
    )
    |> Seq.groupBy(fun hg -> hg.SlotId)
    |> Seq.map (fun (k,items) -> k, items |> List.ofSeq)
    |> List.ofSeq
getHeroInfo()
|> fun (x, _sm) -> x.HeroId
|> getHeroLoot 
|> fun x -> x.Dump()

