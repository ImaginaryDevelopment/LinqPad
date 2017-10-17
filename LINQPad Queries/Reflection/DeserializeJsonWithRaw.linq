<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json</Namespace>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
</Query>

// recursive deserialization with raw meta field


let trim (x:string) = x.Trim()  
let replace (delim:string) (r:string) (x:string) = x.Replace(delim,r)
           
let deserialize<'T> x = JsonConvert.DeserializeObject<'T>(x)
// default binding flags include static, but that's not really what we are after here.
let getPropCI (t:Type) name = match t.GetProperty(name, BindingFlags.Default ||| BindingFlags.IgnoreCase ||| BindingFlags.Public ||| BindingFlags.Instance) with null -> None | x -> Some x

let (|EnumerableT|_|) (t:Type) =
    t.GetInterfaces() |> List.ofSeq |> fun tail -> t::tail
    |> Seq.tryFind(fun x -> x.IsGenericType && x.GetGenericTypeDefinition() = typedefof<Collections.Generic.IEnumerable<_>>)
    |> Option.map (fun t -> t.GenericTypeArguments.[0])
    
let rec maybeAddRaw (t:Type) (jt:JToken):unit = 
    match jt with
    | :? JObject as jo -> 
        //printfn "checking jo"
        // case sensitive? for now.
        match getPropCI t "Raw",jo.Property("raw") with
        | None,_ -> ()
        | Some _,null -> 
            //printfn "found a raw!" 
            let raw = jo |> string
            //(jo |> string).Dump("found raw")
            jo.Add("raw",JToken.FromObject(raw))
        | _, x ->
            printfn "jObject already had a raw"
        // so the object itself is handled, now for child objects
        jo.Properties()
        |> Seq.iter(fun sourceProp -> 
            // what type is the property supposed to be? // and we don't always map all properties
            getPropCI t sourceProp.Name
            |> Option.iter(fun targetProp ->
                maybeAddRaw targetProp.PropertyType sourceProp
            )
        )
        
    | :? JProperty as jp ->
        //printfn "checking jp"
        match jp.Value with
        | null -> ()
        | x -> maybeAddRaw t x
    | :? JArray as ja ->
        match t with
        | EnumerableT itemT ->
            ja
            |> Seq.cast<JToken>
            |> Seq.iter(maybeAddRaw itemT)
        | x -> 
            
            printfn "Could not handle %s as JArray" t.Name 
        
    | :? JValue as jv -> // string or number shouldn't have properties
        ()
    | x -> 
        (x.Type,jt.GetType().Name).Dump("Fail!")
        failwith "bah"
        
let deserializeObjectReflectively<'T>(x:string) :'T = 
    let x = 
        let jt = JsonConvert.DeserializeObject<JToken>(x)
        jt |> maybeAddRaw typeof<'T>
        jt |> string
    //x.Dump("with raw?")
    x
    |> deserialize
    
type SimpleChild={V2:string; N:decimal; Raw:string}
type SimpleGuy={V:string; N:int; Child:SimpleChild; Raw:string; Children:SimpleChild[]; Children2:SimpleChild list list}

let serialized = 
    let simpleChildren = [| {V2="Child1"; N=2m; Raw=null};{V2="Child2"; N=3m; Raw=null}|]
    {V="hello";N=2;Child={V2="world";N=1m; Raw=null};Raw=null; Children=simpleChildren; Children2= [ simpleChildren |> List.ofSeq]}
    |> Newtonsoft.Json.JsonConvert.SerializeObject
    |> replace (""" ,"Raw":null""" |> trim) ""

    
serialized
|> Dump
|> deserializeObjectReflectively<SimpleGuy>
|> Dump
|> ignore