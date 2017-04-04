<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

// purpose: compress the data without fancy library, such that the same algorithm could be used in javascript land
let textPath = @"C:\projects\CotLICheatSheet\testdata\savedAppState.txt"
let dumpt t x = x.Dump(description=t); x
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

module Cereal = 
        
    let (|EmptyItem|_|) (jt:JToken) = 
        match jt with 
        | :? JArray as jv when jv.Count < 1 ->    
            Some ()
        | :? JObject as jo when (jo.Properties() |> Seq.length |> fun l -> l < 1) -> Some ()
        | :? JArray 
        | :? JObject -> None
        | :? JValue as jv when jv.Value = null -> Some ()
        | :? JValue -> None
        | _ -> 
            raise <| Exception ("Unrecognized item type " + jt.GetType().Name + ":" + (string jt))
            
    type Simplicity =
        | Simple of JValue
        | Values of JValue list
        | Objects of JObject list
        | Object of (string*Simplicity) list
//        with
//            static member IsSimple = function | Simple _ -> true | _ -> false
//            static member IsValues = function | Values _ -> true | _ -> false
//            static member IsEmpty = function | Empty -> true | _ -> false

        
    let (|JValueArray|_|) (jt:JToken) = 
        match jt with 
        | :? JArray as jv -> 
            match jv.ToArray() |> Seq.tryCast<JValue> with
            | Some values -> Some values
            | _ -> None
        | _ -> None
        
    type JsonConvert = Newtonsoft.Json.JsonConvert
    let deserialize<'T>(x) = JsonConvert.DeserializeObject<'T>(x) 
    let rec decompose prefix (o:JObject) =
        o.Properties()
        |> Seq.choose (fun p -> 
            match p.Value with
            | EmptyItem -> None
            | :? JValue as jv -> Simple jv |> Some
            | JValueArray jv ->
                Values jv |> Some
            | :? JArray as ja ->
                ja.Values<JObject>()
                |> Seq.cast<JObject>
                |> List.ofSeq
                |> Objects
                |> Some
            | :? JObject as jo ->
                decompose String.Empty jo
                |> List.ofSeq
                |> Object
                |> Some
            | _ -> 
                p.Dump("unmatched")
                raise <| Exception("Hello")
            |> Option.map (fun v -> prefix + p.Name, v)
        )
        |> List.ofSeq
        
open Cereal        
// csv or query value keys for top level stuff?
//let mapCruGear (cruGear :JArray) = 

let fetch () = File.ReadAllText textPath
Util.Cache(fetch,"appStateText")
|> fun x -> x.Length.Dump(); x
|> deserialize<JObject>
|> decompose String.Empty
|> List.ofSeq
|> fun zombies -> 
    zombies 
    |> Seq.choose (fun (name,z) -> match z with | Simple v -> None | _ -> Some(name,z)) 
    |> Dump 
    |> ignore 
    zombies
|> Seq.map(fun (name, simp) ->
    match simp with
    | Simple s -> (s.Value |> string)
    | Values values -> 
        match name with
        |"ownedCrusaderIds" -> 
            values 
            |> Seq.map (fun v -> v.Value) 
            // ditch crusaders that everyone owns
            |> Seq.filter(fun v -> 
                let isNumber,x = System.Int32.TryParse(string v)
                not isNumber || x > 20)
        | _ -> values |> Seq.map (fun v -> v.Value) 
        |> Seq.map string
        |> delimit "-"
    | Object props -> 
        match name with 
        |"crusaderGear" ->                     
            props |> Seq.map (fun (name, v) ->
                let cruId = name
                match v with
                | Object props ->                
                    sprintf "%s:%s" name (props |> Seq.map (fun (slotId,Simple lootId) -> sprintf "%s%A" slotId (lootId.Value |> string |> int)  |>  string) |> delimit "-")
                | _ -> failwith "Bad crusader gear item"
                
            )
            |> delimit ";"

            
        | _ -> props |> Seq.map string |> delimit ","
    | Objects objects -> ""
    |> fun v -> name,v
)
|> Dump
|> Seq.map (fun (n,v) -> sprintf "%s=%s" n (v |> Uri.EscapeDataString))
|> delimit "&"
|> fun x -> x.Length, x
|> Dump
|> ignore