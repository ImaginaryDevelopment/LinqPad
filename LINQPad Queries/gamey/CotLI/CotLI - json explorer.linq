<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

// game data explorer 
// the damn file is just so huge code folding and such don't work in vscode
// sublime works, but you still can't confine a search to a specific subtree, nor jump between trees easily

let cotLIPath = if Directory.Exists(@"D:\projects\CotLICheatSheet\") then @"D:\projects\CotLICheatSheet\js" else @"C:\projects\CotLICheatSheet\js"
[<AutoOpen>]
module Helpers =
    let beforeLast (delim) (x:string) = 
        x.BeforeLast delim
    type String with
        static member startsWith (delim:string) (x:string) =
            x.StartsWith(delim)
    module Seq = 
        // cast one by one, return result only if all items were sucessful
        // not setup for efficiently handling a large number of items
        // result will be reversed if built efficiently?
        let tryCast<'T> (items: IEnumerable) = 
            items
            |> Seq.cast<obj>
            |> Seq.fold (fun castedItems nextItem ->
                match castedItems with
                | Some items ->
                    match nextItem with
                    | :? 'T as t -> 
                        Some (t :: items)
                    | _ -> 
                        None
                | None -> None

                
            ) (Some List.Empty)
            |> Option.map (List.ofSeq>> List.rev)
    module Map =
        let getKeys (map:Map<_,_>) = 
            map
            |> Map.toSeq
            |> Seq.map fst
            |> Set.ofSeq
    type JProperty with
        member x.ToDump() = x.Name
            
    let hoist f x = f x; x
//    let dumpReverse :  (obj) -> unit =
//        let dc = DumpContainer()
//        dc.Dump() |> ignore
//        (fun o -> 
//            match dc.Content with
//            | :? (obj list) as items -> List.Cons(o,items)
//            | _ -> [ o ]
//            |> fun content -> dc.Content <- content
//        )
        
    let (|IsInt|_|) (x:string) = match Int32.TryParse x with | true, x -> Some x |_ -> None
    let flip f y x = f x y
    let inline dumpt (t:string) x = x.Dump(t,exclude="Raw"); x
    let dumpRemoval (items:string seq) x = 
        let x = Util.ToExpando x
        let dic = (box x) :?> IDictionary<string,obj>
        items
        |> Seq.iter(dic.Remove >> ignore<bool>)
    //    x.Dump("duplicateDump")
        x
    let dumpRemoveRaw x = 
        x |> dumpRemoval ["Raw"]
    let (|StartsWithI|_|) (d:string) (s:string) = 
        if s.StartsWith(d, StringComparison.InvariantCultureIgnoreCase) then Some () else None
        
        
[<AutoOpen>]
module JsonHelpers =
    let (|JValueArray|_|) (jt:JToken) = 
        match jt with 
        | :? JArray as jv -> 
            match jv.ToArray() |> Seq.tryCast<JValue> with
            | Some values -> Some values
            | _ -> None
        | _ -> None
    let (|JObjArray|_|) (jt:JToken) =
        match jt with
        | :? JArray as jv ->
            jv.ToArray() |> Seq.tryCast<JObject>
        | _ -> None
    let getStr (name:string) (jo:JObject) = jo.[name] |> string
    let getPropertyNames (jo:JObject) = jo.Properties() |> Seq.map (fun p -> p.Name) |> List.ofSeq
    let getProperty (name:string) (jo:JObject) : JProperty option = jo.Property name |> Option.ofObj
    let getPropertyValue name jo :JToken option = getProperty name jo |> Option.map (fun jp -> jp.Value) |> Option.bind Option.ofObj
    let getPropertyValueJType name jo = getPropertyValue name jo |> Option.map (fun jv -> jv.GetType())
    let getArray name jo = getProperty name jo |> Option.map (fun jp -> jp.Value |> fun x -> x :?> JArray)
    let getArrayT<'T> name jo = getArray name jo |> Option.map (Seq.cast<'T>)

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
            
module GameData = 
    let gameData = lazy(
        Path.Combine(cotLIPath,"gamedata.json")
        |> File.ReadAllText
        |> deserializeJO
    )
    
module Shell = 
    let explorerShow path = 
        Process.Start("explorer.exe", sprintf "/s,%s" path)
        |> fun x -> x.Id.Dump("launched pid - explorer")

open GameData
let getShallowDisplays (ja:JArray) = 
    match ja with
    |JValueArray jva ->
        jva |> Seq.map string |> List.ofSeq |> box
    | x -> sprintf "[..%i]" x.Count |> box    

    
let getShallowJODisplay jo = 
    getPropertyNames jo
    |> Seq.map (fun pn -> 
        match getPropertyValue pn jo with
        | Some (:? JObject as childJo) ->
            match childJo with
            | _ -> 
            // return propname, JType, and returnType
                getPropertyNames childJo
                |> Seq.map (fun pn ->
                    childJo
                    |> getPropertyValue pn
                    |> Option.get
                    |> fun (x:JToken) -> 
                        match x with
                        | :? JValue as jv ->
                            jv.Value
                            |> string
                        | :? JObject ->
                            "obj"
                        | _ -> x.Type |> string
                    |> sprintf "%s:{%s}" pn 
                )

        | Some (:? JValue as jv) -> 
            upcast [ sprintf "%A"jv.Value]
        | Some (:? JArray as ja) ->
            match ja with
            | JValueArray ja ->
                printfn "found jvalue array!"
                ja |> Seq.map (fun jv -> jv.Value |> string)
            | _ -> upcast [ sprintf "[..%i]" ja.Count ]
        | _ -> upcast ["unmapped"]
        |> delimit ",\r\n  "
        |> sprintf "%s:%s" pn
    )
    |> delimit ",\r\n  "
    |> sprintf "{\r\n  %s\r\n}"
    
// this should use the highest level parent in a JObj hierarchy    
let getShallowDisplay (j:JToken) = 
    match j with
    | :? JObject as jo ->
        getShallowJODisplay jo |> box
    | :? JArray as ja ->
        getShallowDisplays ja |> box
    | :? JValue as jv ->
        jv |> string |> box
    | :? JProperty as jp ->
        (jp.Name, jp.Value) |> box
    | _ -> 
        j.Dump("failed") 
        failwithf "Failed to match type in getShallowDisplay"
            
type Explorable = 
    | JObj of JObject
    | JArr of JArray
    with 
        member x.getString() = 
            match x with
            | JObj o ->  box <| getShallowJODisplay o
            | JArr ja -> box <| getShallowDisplays ja
            
// navigate via commands

let exportSubTree name = 
    let path = Path.Combine(cotLIPath,sprintf "%s.json" name)
    (fun x ->
        match x with
        | JObj x -> x :> JContainer
        | JArr x -> x :> JContainer
        |> fun x -> 
            File.WriteAllText(path, x |> string)
            Shell.explorerShow path
    )        
    
let loopCommands () = 

    
    //command status
    let statusDc : string -> unit = 
        DumpContainer()
        |> Dump
        |> fun dc ->
            (fun (x:string) ->
                dc.Content <- box x
            )
    let getCurrentObj, setCurrentObj, getCurrentPath, refreshDisplay, setDisplay= 
        
        let root = GameData.gameData.Value
        let mutable currentObj : Explorable = JObj root
        let getCurrentPath,setCurrentPath = 
            let mutable currentPath = ""
            let display() = sprintf "CurrentPath: %s" currentPath
            DumpContainer(display())
            |> Dump
            |> fun dc ->
                let fGet() = currentPath
                let fSet (x:string) = 
                    currentPath <- x
                    dc.Content <- display()
                fGet,fSet
        DumpContainer()
        |> Dump
        |> fun dc ->
            let fGet () = currentObj
            let fSet path (x:Explorable)  =
                currentObj <- x
                dc.Content <- x.getString()
                setCurrentPath path
            fSet (getCurrentPath()) currentObj
            fGet,fSet, getCurrentPath, 
                (fun () -> fSet (getCurrentPath()) currentObj), 
                (fun (disp:obj) -> dc.Content <- disp)
            
    
    let mutable input = ""
    // msg display/response to bad commands output
    
    // current obj output
    
    
    let commands = 
        Map [
            "export %s", (fun (args:string) -> exportSubTree args (getCurrentObj()) |> ignore)
            "quit", fun _ -> input <- null
            "ascend",
                fun _ -> 
                    let cp = getCurrentPath()
                    if cp |> String.contains "." |> not || cp = "." then
                        "Already at root, can't ascend"
                        |> setDisplay
                    else                        
                        let newPath = getCurrentPath() |> beforeLast "."
    
                        getCurrentObj()
                        |> function
                            |JObj jo -> jo.Parent
                            |JArr ja -> ja.Parent 
                            |> function
                                | :? JProperty as jp -> 
                                    // double parenting eh?
                                    jp.Parent
                                    |> function
                                        | :? JObject as jo -> JObj jo 
                                        | :? JArray as ja -> JArr ja
                                | :? JArray as ja ->
                                    JArr ja
                                    
                                | x -> 
                                    x.Dump("ascending into failure")
                                    failwithf "unexpected type to ascend into %s" (x.GetType().Name)
                        |> setCurrentObj newPath
            // replace the display with a custom
            "select %s", fun args ->
                getCurrentObj()
                |> function
                    |JArr (JObjArray items) -> 
                        let arg = args |> beforeOrSelf " "
                        let getPropertyValues item =
                            args 
                            |> after " "
                            |> String.split [","]
                            |> Seq.map (fun x -> getPropertyValue (x.Trim()) item |> Option.map (getShallowDisplay >> string) |> Option.getOrDefault "")
                            |> delimit ","
                            
                        items
                        |> Seq.mapi (fun i item ->
                            item
//                            |> getPropertyValue arg
//                            |> Option.map (getShallowDisplay >> (fun x -> sprintf "%i,%A"i x))
                            |> getPropertyValues
                            |> sprintf "%i, %A" i
                        )
                        |> setDisplay 
                        getCurrentPath()
                        |> sprintf "Showing (index,%s) property of %s" arg
                        |> statusDc
                    | JObj _ -> statusDc "use descend on object properties, select is for array item props"
            "show", 
                function
                | null | "" ->
                    statusDc "show requires an int limit to function"
                | bounds ->
                    // TODO: feature show via skip truncate instead of only truncate
//                    let upper,lower =
//                        bounds
//                        |> String.split [" "]
//                        |> function
//                            | [upper] -> upper, None
//                            | [
                    match getCurrentObj(),System.Int32.TryParse limit with
                    | _,(false,_) -> statusDc "show requires an int limit to function"
                    | JArr(JObjArray items), (_,limit) ->
                        items
                        |> Seq.truncate limit
                        |> Seq.map string
                        |> setDisplay
                    | JArr(items), (_,limit) ->
                        items
                        |> Seq.truncate limit
                        |> Seq.map string
                        |> setDisplay
                    | _ -> statusDc "show requires the target be an array to function"
                    
            "filter", 
                function
                | null | "" ->
                        statusDc "filter requires an argument(propName,value) to function"
                | args ->
                    match args.Split(' ') |> List.ofSeq with
                    | [propName;value] ->
                        getCurrentObj()
                        |> function
                            |JArr (JObjArray items) ->
                                items
                                |> Seq.filter(fun item ->
                                    item
                                    |> getPropertyValue propName
                                    |> Option.map (string >> (=) value)
                                    |> Option.getOrDefault false
                                )
                                |> Seq.map string
                                |> setDisplay
                            | _ -> statusDc "filter requires the current array be an object array"
                    | _ -> statusDc "filter requires a propName and value to function"
                
            // refresh the display back to the currentPath object
            "refresh", fun _ -> 
                refreshDisplay()
            // descend %i for arrays
            "descend %s", 
                function
                | null | "" -> 
                    statusDc "descend requires an argument to function"
                | propName ->
                    let nextPath = sprintf "%s.%s" (getCurrentPath()) propName
                    statusDc <| sprintf "attempting descent into %s" nextPath
                    // once we have the value we are descending into run it
                    let trySetExpl name (jt:JToken)= 
                        match jt with
                        | :? JObject as o ->
                            JObj o
                        | :? JArray as ja ->
                            JArr ja
                        | x -> 
                            printfn "currentPath was not a happy one %s" <| getCurrentPath()
                            x.GetType().Name
                            |> failwithf "unexpected type %s" 
                        |> setCurrentObj nextPath
                        
                    match getCurrentObj() with
                    | JObj x ->
                        // unless it has object or array children, descending into it doesn't make sense
                        x 
                        |> getPropertyValue propName 
                        |> function
                            | Some value ->
                                trySetExpl propName value
                            | None -> 
                                
                                sprintf "could not descend into %s.%s it was empty" 
                                    <| getCurrentPath() 
                                    <| propName
                                |> statusDc
                        
                        
                    | JArr ja ->
                        ja.[int propName]
                        |> trySetExpl propName
                        
                        
                        
        ]               
    
    printfn "Starting loops!"
    while not <| isNull input do
        input <- Util.ReadLine("command?", null, commands |> Map.getKeys |> Seq.map (beforeOrSelf " "))
        match input with
        | "" ->
            ()
        | _ ->
            let cmd = input |> beforeOrSelf " "
            let args = input |> afterOrSelf " "
            commands
            |> Map.toSeq
            |> Seq.tryFind(fun (k,_) -> cmd = k || k |> beforeOrSelf " " |> String.startsWith cmd)
            |> function
                | None -> sprintf "invalid input or command not found: %s" input |> statusDc 
                | Some (_, f) -> 
                    f args
        ()
    

loopCommands()