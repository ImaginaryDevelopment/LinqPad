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
    let dic = x :?> IDictionary<string,obj>
    items
    |> Seq.iter(dic.Remove >> ignore<bool>)
    x.Dump("duplicateDump")
    x
let dumpRemoveRaw x = 
    x |> dumpRemoval ["Raw"]
let getStr (name:string) (jo:JObject) = jo.[name] |> string
let hasProperty (name:string) (jo:JObject) = jo.Property(name) |> isNull |> not
let setProperty (name:string) (value:obj) jo = if jo |> hasProperty name then jo.Property(name).Value <- JToken.FromObject(value) else jo.Add(name,JToken.FromObject value)
type MissionTag={Id:string; DisplayName:string; Image:string; Raw:JObject} with member x.ToDump() = x |> dumpRemoveRaw
        
type Crusader= { Id:string;Link:string;DisplayName:string; Raw:JObject} with member x.ToDump() = x |> dumpRemoveRaw

type CrusaderData = {Wikibase: string; MissionTags: MissionTag list; Crusaders: Crusader list; Raw:JObject} with member x.ToDump() = x |> dumpRemoveRaw
        
let mapMissionTag (jo:JObject) = 
    {Id = jo.["id"] |> string; DisplayName= jo.["displayName"] |> string; Image= jo.["image"] |> string; Raw=jo}
let mapCrusader (jo:JObject) = 
    {   Id = jo.["id"] |> string
        DisplayName= jo.["displayName"] |> string
        Link = getStr "link" jo
        Raw = jo
        }
let (|StartsWithI|_|) (d:string) (s:string) = 
    if s.StartsWith(d, StringComparison.InvariantCultureIgnoreCase) then Some () else None
let mutable hasChanges = false
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
|> setEmptyLinks
|> hoist dumpReverse
|> fun x -> Newtonsoft.Json.JsonConvert.SerializeObject(x.Raw, Newtonsoft.Json.Formatting.Indented)
|> fun x -> sprintf "%s%s%s" starter x trailer
|> fun x -> File.WriteAllText(path, x)
//|> dumpReverse
