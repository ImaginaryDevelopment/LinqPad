<Query Kind="FSharpExpression">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json</Namespace>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

// process some item json, looking for commonalities or tells for if something is a jewel
// json from https://www.pathofexile.com/character-window/get-items?character=DontLetMeGetMe&accountName=DevelopersDevelopersDevelopers


let replace (d:string) (r:string) (x:string) = x.Replace(d,r)
let getProp name (jo:JObject)= jo.Property name
let getProps (jo:JObject)= jo.Properties()
let getProps1 names (jo:JObject) = names |> List.map(fun n -> jo.Property n)
let getValue (jp:JProperty) = jp.Value
let propToStr (jp:JProperty) = jp.Name, box (jp.Value |> string)

// works on objects not arrays
let deserializeObjDict(x) = JsonConvert.DeserializeObject<IDictionary<string,JToken>>(x)
let inline dumpLength title (x:_ list) = 
    x.Length.Dump(description=title)
    x
let tupleFromF f x = 
    f x,x
let inventoryIdKnown x =
    let result = 
        [ "Ring";"Ring2"; "Gloves"; "Belt"; "Amulet";"Weapon2";"Helm";"Boots";"Offhand";"Offhand2";"BodyArmour";"Flask";"Weapon"]
        |> Seq.fold(fun y item -> y || item = x) false
    //printfn "%s isKnown? %A" x result 
    result
let isJewel (x:JObject) = 
    x |> getProp "h" |> getValue |> int |> (=) 1
        && x |> getProp "w" |> getValue |> int |> (=) 1

"""{"hashes":[1031,1203,1346,1822,2292,2913,3656,4011,4397,4502,5152,5823,5865,9271,9355,9877,10763,10808,11420,11551,13009,13714,13885,14936,15073,15405,15868,16790,17735,18707,18769,19635,19939,20551,21835,21958,22627,24050,24383,24528,25058,25757,26523,26866,27203,28012,28754,31875,32710,33631,33740,33755,33989,35283,35296,35894,36047,36858,37663,38176,38190,38999,39861,41472,41635,42583,44606,44723,44908,46408,46910,47507,48362,48904,49254,49651,49929,49978,50862,50904,53279,53493,55485,58449,58968,60398,60405,61198,61419,61471,61653,61981,62021,63048,63583,65034,65167],"items":[{"verified":false,"w":1,"h":1,"ilvl":80,"icon":"https:\/\/web.poecdn.com\/image\/Art\/2DItems\/Jewels\/basicstr.png?scale=1&scaleIndex=0&w=1&h=1&v=5496129c557831c118a679c1001f3df93","league":"Harbinger","id":"0474c11f92ab9cb1f23ee3c24ea6e7448b8759d59a4787756b9a1358b82098a1","sockets":[],"name":"<<set:MS>><<set:M>><<set:S>>Empyrean Creed","typeLine":"Crimson Jewel","identified":true,"corrupted":false,"lockedToCharacter":false,"explicitMods":["9% increased Global Critical Strike Chance","6% increased maximum Life","14% increased Mana Regeneration Rate"],"descrText":"Place into an allocated Jewel Socket on the Passive Skill Tree. Right click to remove from the Socket.","frameType":2,"x":19,"y":0,"inventoryId":"PassiveJewels","type":"JewelStr","socketedItems":[]},{"verified":false,"w":1,"h":1,"ilvl":80,"icon":"https:\/\/web.poecdn.com\/image\/Art\/2DItems\/Jewels\/basicdex.png?scale=1&scaleIndex=0&w=1&h=1&v=7375b3bb90a9809870b31d1aa4aa68b93","league":"Harbinger","id":"55d6f260db5b8d2a09f110a85729f17c0d8d7dc4d08699e068b4477be68b7dd0","sockets":[],"name":"<<set:MS>><<set:M>><<set:S>>Kraken Bliss","typeLine":"Viridian Jewel","identified":true,"corrupted":false,"lockedToCharacter":false,"explicitMods":["7% increased Accuracy Rating","7% increased Global Critical Strike Chance","6% increased maximum Life","+15% to Cold Resistance"],"descrText":"Place into an allocated Jewel Socket on the Passive Skill Tree. Right click to remove from the Socket.","frameType":2,"x":2,"y":0,"inventoryId":"PassiveJewels","type":"JewelDex","socketedItems":[]},{"verified":false,"w":1,"h":1,"ilvl":79,"icon":"https:\/\/web.poecdn.com\/image\/Art\/2DItems\/Jewels\/basicint.png?scale=1&scaleIndex=0&w=1&h=1&v=cd579ea22c05f1c6ad2fd015d7a710bd3","league":"Harbinger","id":"e18c2bd05e72a2ef3e7087603580df2950bf2127d723048985f8d15d85d06ab3","sockets":[],"name":"<<set:MS>><<set:M>><<set:S>>Behemoth Shard","typeLine":"Cobalt Jewel","identified":true,"corrupted":false,"lockedToCharacter":false,"explicitMods":["7% increased maximum Life","2% increased Attack and Cast Speed","+12% to Cold and Lightning Resistances"],"descrText":"Place into an allocated Jewel Socket on the Passive Skill Tree. Right click to remove from the Socket.","frameType":2,"x":14,"y":0,"inventoryId":"PassiveJewels","type":"JewelInt","socketedItems":[]}],"jewel_slots":[{"id":"jewel_slot1956","passiveSkill":{"id":"jewel_slot1956","hash":"26725","name":"Jewel Socket"}},{"id":"jewel_slot1957","passiveSkill":{"id":"jewel_slot1957","hash":"36634","name":"Jewel Socket"}},{"id":"jewel_slot1958","passiveSkill":{"id":"jewel_slot1958","hash":"33989","name":"Jewel Socket"}},{"id":"jewel_slot1959","passiveSkill":{"id":"jewel_slot1959","hash":"41263","name":"Jewel Socket"}},{"id":"jewel_slot1960","passiveSkill":{"id":"jewel_slot1960","hash":"60735","name":"Jewel Socket"}},{"id":"jewel_slot1961","passiveSkill":{"id":"jewel_slot1961","hash":"61834","name":"Jewel Socket"}},{"id":"jewel_slot1963","passiveSkill":{"id":"jewel_slot1963","hash":"31683","name":"Jewel Socket"}},{"id":"jewel_slot1964","passiveSkill":{"id":"jewel_slot1964","hash":"28475","name":"Jewel Socket"}},{"id":"jewel_slot1966","passiveSkill":{"id":"jewel_slot1966","hash":"6230","name":"Jewel Socket"}},{"id":"jewel_slot1967","passiveSkill":{"id":"jewel_slot1967","hash":"48768","name":"Jewel Socket"}},{"id":"jewel_slot1968","passiveSkill":{"id":"jewel_slot1968","hash":"34483","name":"Jewel Socket"}},{"id":"jewel_slot1969","passiveSkill":{"id":"jewel_slot1969","hash":"7960","name":"Jewel Socket"}},{"id":"jewel_slot1970","passiveSkill":{"id":"jewel_slot1970","hash":"46882","name":"Jewel Socket"}},{"id":"jewel_slot1971","passiveSkill":{"id":"jewel_slot1971","hash":"55190","name":"Jewel Socket"}},{"id":"jewel_slot1972","passiveSkill":{"id":"jewel_slot1972","hash":"61419","name":"Jewel Socket"}},{"id":"jewel_slot1974","passiveSkill":{"id":"jewel_slot1974","hash":"2491","name":"Jewel Socket"}},{"id":"jewel_slot1975","passiveSkill":{"id":"jewel_slot1975","hash":"54127","name":"Jewel Socket"}},{"id":"jewel_slot1976","passiveSkill":{"id":"jewel_slot1976","hash":"32763","name":"Jewel Socket"}},{"id":"jewel_slot1977","passiveSkill":{"id":"jewel_slot1977","hash":"26196","name":"Jewel Socket"}},{"id":"jewel_slot1978","passiveSkill":{"id":"jewel_slot1978","hash":"33631","name":"Jewel Socket"}},{"id":"jewel_slot1979","passiveSkill":{"id":"jewel_slot1979","hash":"21984","name":"Jewel Socket"}}]}"""
|> deserializeObjDict
|> fun x -> x.["items"] :?> JArray
|> Seq.cast<JObject>
|> List.ofSeq
|> dumpLength "Total items"

|> Seq.filter(getProp "inventoryId" >> getValue >> string >> inventoryIdKnown >> not)
|> List.ofSeq
|> dumpLength "Included"
//|> List.map (getProps1 ["name";"inventoryId"]>>List.map (getValue >> string))
//|> Seq.truncate 10
|> List.filter isJewel
|> dumpLength "Jewels?"

//|> Seq.map(tupleFromF (getProp "name">> getValue >> string))
|> Seq.map (getProps >> Seq.map propToStr)
