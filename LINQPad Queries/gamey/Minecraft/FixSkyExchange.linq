<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

// https://www.reddit.com/r/feedthebeast/comments/8wjhs0/skyexchange_broken_quest/
let skyExchangePath = 
    let x = Path.Combine(Environment.ExpandEnvironmentVariables("%userprofile%"),"My Documents\Curse\Minecraft\Instances\Skyexchange")
    x
let pathToQuestDb =
    let x = Path.Combine(skyExchangePath,"saves\A Hole New World\QuestDatabase.json")
    if not <| File.Exists x then failwithf "Questdb not found at %s" x
    if Path.GetExtension x <> ".json" then failwithf "Questdb format unexpected %s" (Path.GetFileName x)
    x
let pathToSkyExchange = 
    let x = Path.Combine(skyExchangePath, "config\CraftingHarmonics\Sets\drops.json")
    x
    
let o1 = JObject.Parse <| File.ReadAllText pathToQuestDb
let getProp name (x:JObject) = x.Property(name) |> Option.ofObj
let getPropValue name x = getProp name x |> Option.bind(fun p -> p.Value|> Option.ofObj) 
let castAs<'t>(x:obj) = x :?> 't
let getPropArray name x = getPropValue name x |> Option.map castAs<JArray>

let fixTag (x:JObject) =
    x
    |> getPropValue "tag"
    |> Option.bind (castAs<JObject> >> getProp "orig_id")
    |> Option.iter(fun prop ->
        prop.Value
        |> Option.ofObj
        |> Option.iter(fun orig ->
            let oid = orig.ToString()
            if oid.StartsWith "extrautils2:" then
                let next = oid.ToLowerInvariant()
                (oid,next).Dump("Changing")
                prop.Value <- JToken.FromObject next
                
        )
    )
let fixQuestIcon (quest:JObject) =
    quest.Dump("icon?")
    getPropValue "icon" quest
    |> Option.iter(castAs<JObject> >> fixTag)
    
let fixTask (task:JObject) =
    getPropArray "requiredItems" task
    |> Option.iter (Seq.iter (castAs<JObject> >> fixTag))
o1
|> getPropArray "questDatabase"
|> Option.iter (Seq.cast<JObject>  >> Seq.iter(fun quest ->
        fixQuestIcon quest
        quest
        |> getPropArray "tasks"
        |> Option.iter (Seq.cast<JObject> >> Seq.iter fixTask)
    )
)
File.WriteAllText(pathToQuestDb, o1.ToString())

printfn "Wrote to %s" pathToQuestDb
