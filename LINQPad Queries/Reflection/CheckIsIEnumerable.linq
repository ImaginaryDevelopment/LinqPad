<Query Kind="FSharpProgram" />

// reflection on list/array/seq/etc..


let items = [
    box [
        1,2
        2,3
    ]
    box [
        "1","2"
    ]
    box [|
        1,2
    |]
    box [|
        3,4
    |]
    box "hello"
    seq{
        printfn "iterating!"
        yield 1
        printfn "finished"
    } |> box
    box (dict ["test",1])
    box (["test";"test"] |> Set.ofSeq)    
]
let (|EnumerableT|_|) (x:obj) = 
    x.GetType().GetInterfaces() |> Seq.tryFind(fun x -> x.IsGenericType && x.GetGenericTypeDefinition() = typedefof<Collections.Generic.IEnumerable<_>>)
    |> Option.map (fun t -> t.GenericTypeArguments.[0])

items
|> Seq.map (fun item -> item, match item with |EnumerableT t -> Some t | _ -> None)
|> Dump
|> ignore
let getTypes (t:Type) = 
    //printfn "Checking type %s" (if isNull t then "null" else t.Name)
    t
    |> Seq.unfold(fun t ->
        if isNull t ||  t = typeof<obj> then
            //printfn "ignoring/stopping unfold"
            None
        else 
            //printfn "found a type %s" t.Name
            Some(t, t.BaseType)
    )
    //|> Dump
let getInterfaces(t:Type) = 
    t.GetInterfaces()
    |> List.ofArray
    
let getAllTypes (t:Type) = 
    getTypes t
    |> Seq.map(fun t -> t::(t.GetInterfaces() |> List.ofArray))
let getIsIEnumerableT (t:Type) = 
    if isNull t then
        failwith "getIsIEnumerable 'null'"
    else
        t.IsArray || t = typedefof<System.Collections.Generic.IEnumerable<_>> || t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<IEnumerable<_>> || t.GetInterfaces() |> Seq.exists (fun t -> t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<IEnumerable<_>>)
        
let getIsIEnumerable (t:Type) = 
    if isNull t then
        failwith "getIsIEnumerable 'null'"
    else
        t.IsArray || t = typedefof<System.Collections.IEnumerable> || t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<IEnumerable<_>> || t.GetInterfaces() |> Seq.exists (fun t -> t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<IEnumerable<_>>)
let getType (x:obj) = x.GetType()
let checkers = [
    (fun (v:obj) -> "get",[getType v,getType v |> getIsIEnumerableT])
    (fun (v:obj) -> "getParents", getType v |> getTypes |>  Seq.map (fun t -> t,getIsIEnumerableT t) |> List.ofSeq)
    (fun v -> "getAllTypes", getType v |> getAllTypes |> Seq.concat |> Seq.map (fun t -> t,getIsIEnumerableT t) |> List.ofSeq)
    (function 
        | :? Collections.Generic.IEnumerable<obj> as v -> "downcasting", [getType v,true] 
        | v -> "downcast", [getType v,false]
    )
]
items
|> Seq.map(fun item ->
    item, 
        checkers
        |> Seq.map(fun f -> item.GetType().IsClass, f item)
        
)
//|> Seq.map(fun x ->
//    let t = x.GetType()
//    getTypes , x
//)
|> Dump
|> ignore