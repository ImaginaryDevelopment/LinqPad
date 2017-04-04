<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

// purpose: compress the data without fancy library, such that the same algorithm could be used in javascript land
let textPath = @"C:\projects\CotLICheatSheet\testdata\savedAppState.txt"

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
//    let tryConvertToNumOrStrings (items: IEnumerable) = 
        
module Cereal = 
    type Simplicity =
        | Simple of JValue
        | Values of JValue list
        | Objects of JObject list
        | Object of (string*Simplicity) list

//    let (|JValue|_|) (jp:JProperty) = 
//        match jp.Value with
//        | :? JValue as jv -> Some jv
//        | _ -> None
        
    let (|JValueArray|_|) (jt:JToken) = 
        match jt with 
        | :? JArray as jv -> 
            match jv.ToArray() |> Seq.tryCast<JValue> with
            | Some values -> Some values
            | _ -> None
        | _ -> None
//    let (|JObjArray|_|) (jp:JProperty) = 
//        match jp.Value with
//        | :?
        
    type JsonConvert = Newtonsoft.Json.JsonConvert
    let deserialize<'T>(x) = JsonConvert.DeserializeObject<'T>(x) 
    let rec decompose prefix (o:JObject) =
        o.Properties()
        |> Seq.map (fun p -> 
            match p.Value with
            | :? JValue as jv -> Simple jv
            | JValueArray jv ->
                Values jv
            | :? JArray as ja ->
                ja.Values<JObject>()
                |> Seq.cast<JObject>
                |> List.ofSeq
                |> Objects
            | :? JObject as jo ->
                decompose String.Empty jo
                |> List.ofSeq
                |> Object
            | _ -> 
                p.Dump("unmatched")
                raise <| Exception("Hello")
            |> fun v -> prefix + p.Name, v
        )
        |> List.ofSeq
        
open Cereal        
// csv or query value keys for top level stuff?
//let mapCruGear (cruGear :JArray) = 
    
File.ReadAllText(textPath)
|> deserialize<JObject>
|> decompose String.Empty
//|> Seq.map(fun (name, simp) ->
//    match simp with
//    | Simple s -> sprintf "%s=%s" name (Uri.EscapeDataString s)
//    | Complex c -> 
//        match name, c with
//        | "crusaderGear", ( :? JArray as cruasderIdsToGear), _ -> 
//            cruasderIdsToGear    
//            |> Array.map(fun cruProp ->
//                cruProp
//                )
//)
//|> string
//|> Seq.take 100
//|> string
|> Dump
|> ignore