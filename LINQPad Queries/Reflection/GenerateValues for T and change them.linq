<Query Kind="FSharpProgram" />

// generate values function
// purpose: a function for generating values for any simple property, and a sample consumer/test

let isType<'a> = Unchecked.defaultof<'a>
module Reflection = 
    [<RequireQualifiedAccess>]
    module Type = 
    // this function didn't work: https://stackoverflow.com/questions/52005448/whats-wrong-with-activepattern-matching-against-system-type
//        let (|IsEqual|Isnt|) (_:'a) (t:Type):Choice<unit,unit> =
//            let t' = typeof<'a>
//            if t = t' then IsEqual else Isnt
        let (|Equals|_|) (_:'a) (t:Type) :unit option =
            if t = typeof<'a> then Some ()
            else
                //printfn "did not match %A to %A" typeof<'a> t
                None
                
        // if you want to know an instance of System.Type is Nullable, gives the T
        let (|NullableT|_|) (t:Type) =
            if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Nullable<_>> then
                Some t.GenericTypeArguments.[0]
            else None
        
        let (|NullableValueT|_|) (v,t) : obj option =
            match t with
            | NullableT t -> Some t
            | _ -> None
            |> Option.bind(fun t ->
                let inline (|NotNull|IsNull|) (x:obj) = match x with null -> IsNull |_ -> NotNull
                match v with
                |NotNull ->
                    Some null
                |IsNull ->
                    match t with 
                    | Equals(isType:int) -> Nullable 2 |> box |> Some
                    | Equals(isType:DateTime) -> DateTime.MinValue |> box |> Some
                    | Equals(isType:bool) -> true |> box |> Some
                    | _ -> None
            )
        let getTypeString =
            function
            | NullableT t -> sprintf "Nullable<%s>" t.Name
            | t when t.IsValueType -> t.Name
            | Equals (isType:Guid)
            | Equals (isType:String)
                as t -> t.Name
            | x -> 
                printfn "No print type for %s" x.FullName
                x.Name


open Reflection


let getChangePropertyF (prop:PropertyInfo) (x:'a) : (unit -> unit) option =
    match prop.GetValue(x), prop.PropertyType with
    | null, Type.Equals (isType:string) ->
        "hello" |> box |> Some
    | :? string as x, Type.Equals(isType:string) ->
        if x = "hello" then "world" else "hello"
        |> box
        |> Some
    | Type.NullableValueT x -> box x |> Some
    | :? int as i, Type.Equals (isType:int) ->
        let x = [1..3] |> Seq.find((<>) i)
        Some <| box x
    | ( :? bool as x), Type.Equals(isType:Nullable<bool>)
    | ( :? bool as x), Type.Equals(isType:bool) ->
        Some <| box (not x)
    | ( :? decimal as x), Type.Equals(isType:Nullable<decimal>)
    | ( :? decimal as x), Type.Equals(isType:decimal) ->
        Some <| box (x + 1M)

    | :? DateTime as x, Type.Equals(isType:DateTime) ->
        if x = DateTime.MinValue then
            DateTime.MaxValue
        else DateTime.MinValue
        |> box
        |> Some
    | :? Guid as x, Type.Equals(isType:Guid) ->
        let y = Guid.NewGuid()
        if x = y then
            Guid.NewGuid()
        else y
        |> box
        |> Some

    | null, Type.Equals(isType:byte[]) ->
        Some <| box Array.empty<byte>
    | null, pt ->
        printfn "No match for %s" <| Type.getTypeString pt
        None
    | vt,pt ->
        //printfn "No match for %s,%s" (getTypeString <| vt.GetType()) (getTypeString pt)
        None
    |> Option.map(fun value -> fun () ->
        prop.SetValue(x,value)
    )

[<CLIMutable>]
type SampleRec = {AString:string;ABool:bool;AnInt:int;ADec:decimal}
//type Sample() = 
//    member val AString = String.Empty
//    member val ABool = false
//    member val AnInt = 1
    
type SampleTestResult = {PropName:string; TestPassed:bool;OldValue:obj;NewValue:obj}
let x = {AString="Test";ABool=false;AnInt=1;ADec=1M}

typeof<SampleRec>.GetProperties()
|> Array.map(fun prop ->
    getChangePropertyF prop x
    |> function
        |Some f -> 
            let oldValue = prop.GetValue(x)
            f()
            let value = prop.GetValue(x)
            {PropName=prop.Name;TestPassed= value <> oldValue;OldValue=oldValue;NewValue=value}
        | None -> invalidOp <| sprintf "could not find a value for %s with type %s" prop.Name prop.PropertyType.Name
)
|> Dump
|> ignore