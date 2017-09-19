<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

// test custom deserializer reflection
module Helpers =
    let trim (x:string) = x.Trim()  
    let replace (delim:string) (r:string) (x:string) = x.Replace(delim,r)
    let tee<'T> f (x:'T) = 
        f x |> ignore;
        x
//    let dumpReverse  =
//        let dcF = DumpContainer() |> Dump
//        let dc = DumpContainer()
//        dc.Dump() |> ignore
//        (fun dumpRevType o -> 
//            match dumpRevType with
//            | Cont -> 
//                match dc.Content with
//                | :? (obj list) as items -> List.Cons(o,items)
//                | _ -> [ o ]
//                |> fun content -> dc.Content <- content
//            | Final ->
//                dcF.Content <- o
//        )
open Helpers
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

module Serialization = 
    open Newtonsoft.Json
    let deserialize (t:Type) (x:string) = JsonConvert.DeserializeObject(x,t)
    let deserializeT<'T> (x:string) = JsonConvert.DeserializeObject<'T>(x)
    let (|JValueArray|_|) (jt:Linq.JToken) = 
        match jt with 
        | :? Linq.JArray as jv -> 
            match jv.ToArray() |> Seq.tryCast<Linq.JValue> with
            | Some values -> Some values
            | _ -> None
        | _ -> None
    // consider this to ignore serialization of properties when the name = raw
    //type private JsonIgnoreRawResolver =
    let private processRecordProp tName raw (jo:Linq.JObject) (p:PropertyInfo) result f :bool*obj =
        match jo.Property(p.Name) with 
        | null -> false,p.GetValue(result) 
        | x ->
            printfn "checking child property for recursion need %s.%s : %s" tName p.Name p.PropertyType.Name
            match x.Value with
            | :? Linq.JValue as jv -> 
                printfn "Not recursing %s" p.Name
                false,p.GetValue(result)
            | :? Linq.JObject as joChild ->
                
                let hasChanges,value = joChild |> string |> tee (printfn "recursing %s") |> f p.PropertyType
                hasChanges,value
            | :? Linq.JArray as ja ->
                // target type might be list, or array, what to do?
                ja |> string |> f p.PropertyType                
            //| :? Linq.JToken as jt -> failwithf "unsupported type %A" jt.Type
            | y -> failwithf "could not process %s : %s" x.Name (y.Type.ToString())
    
    let private processRecordWithRaw (t:Type) raw f props (pRaw:PropertyInfo) (jo:Linq.JObject) result = 
        printfn "Making record"
        let nextProps = 
            props 
            |> Seq.map(fun p -> 
                if p<>pRaw then 
                    // doesn't really matter if we have other prop changes or not, we are changing raw
                    let _, value = processRecordProp t.Name raw jo p result f
                    p.Name, value                            
                else p.Name,box raw) 
            |> Array.ofSeq
            
        printfn "Properties = %A" nextProps
        nextProps.Dump("nextProps")
        let result = FSharp.Reflection.FSharpValue.MakeRecord(t,nextProps |> Array.map snd)
        true,result
    let getTypes (t:Type) = 
        t
        |> Seq.unfold(fun t ->
            if not<| isNull t.BaseType && t.BaseType <> typeof<obj> then
                None
            else Some(t.BaseType, t.BaseType)
        )
    let (|EnumerableT|_|) (x:obj) = 
        x.GetType().GetInterfaces() |> Seq.tryFind(fun x -> x.IsGenericType && x.GetGenericTypeDefinition() = typedefof<Collections.Generic.IEnumerable<_>>)
        |> Option.map (fun t -> t.GenericTypeArguments.[0])
    let getIsIEnumerable (t:Type) = 
        if isNull t then
            failwith "getIsIEnumerable 'null'"
        else
            t.IsArray || t = typedefof<System.Collections.IEnumerable<_>> || t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<IEnumerable<_>> || t.GetInterfaces() |> Seq.exists (fun t -> t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<IEnumerable<_>>)
    let deserializeObjectReflectively<'T>(x:string) = 
        let rec deserializeR(t:Type) x :bool*obj = 
            let isRecord = Microsoft.FSharp.Reflection.FSharpType.IsRecord t
            let props = t.GetProperties() |> List.ofSeq |> tee (Seq.map(fun p -> p.Name) >> Dump >> ignore)
            // skip recursion into child props for right now, get outer working first
            let constructed = JsonConvert.DeserializeObject(x,t)
            if t.IsClass then
                match JsonConvert.DeserializeObject<Linq.JToken>(x) with
                | JValueArray ja ->
                    failwith ""
                | :? Linq.JValue as jv -> 
                    printfn "JValue: %A" jv.Value
                    false,jv.Value 
                | :? Linq.JArray as ja ->
                    match constructed with
                    |EnumerableT t ->
                        let processed = 
                            values
                            |> Seq.map (deseralizeR t)
                        if processed |> Seq.exists fst then
                            // need to reconstruct the right type here
                            true, procesed
                
                
                | :? Linq.JObject as jo ->
                    
                    let raw = x
                    printfn "JObject: %s" raw
                    
                    let result =  raw |> deserialize t
                    result.Dump("checking/working raw")
                    match props |> Seq.tryFind(fun p -> p.Name = "Raw" && p.PropertyType = typeof<string> || p.PropertyType = typeof<obj>) with
                    | Some pRaw when pRaw.PropertyType = typeof<string> -> 
                        if isRecord then
                            processRecordWithRaw t raw deserializeR props pRaw jo result
                        else        
                            printfn "Setting raw string"
                            pRaw.SetValue(result,raw)
                            true,result
                    | Some p -> 
                        printfn "setting raw obj"
                        p.SetValue(result,Util.OnDemand("Raw",fun () -> raw))
                        true,result
                    | None -> 
                        result.Dump("item!")
                        false,result
            else
                failwith "oopsies"
        let t = typeof<'T>
        deserializeR t x |> snd :?> 'T
    
    let deserialize2<'T>(x:string) : 'T =
        deserializeObjectReflectively<'T> x
open Serialization

type SimpleChild={V2:string; N:decimal; Raw:string}
type SimpleGuy={V:string; N:int; Child:SimpleChild; Raw:string; Children:SimpleChild[]}

let serialized = 
    Newtonsoft.Json.JsonConvert.SerializeObject( {V="hello";N=2;Child={V2="world";N=1m; Raw=null};Raw=null; Children=[| {V2="Child1"; N=2m; Raw=null}|]})
    |> replace (""" ,"Raw":null""" |> trim) ""

serialized
|> Dump
|> deserializeObjectReflectively<SimpleGuy>
|> Dump
|> ignore