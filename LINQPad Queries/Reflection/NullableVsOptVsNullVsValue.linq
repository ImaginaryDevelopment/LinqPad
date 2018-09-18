<Query Kind="FSharpProgram" />

let x = box <| Nullable(true)
let isOpt (x:obj) = x |> function | :? Option<bool> as x when Option.isSome x -> "yes:Some" | :? Option<bool> -> "yes:None" | _ -> "no"
let isN (x:obj) = x |> function | :? Nullable<bool> -> true | _ -> false
let isV (x:obj) = x |> function | :? bool -> true | _ -> false
type TestResult = {Name:string; IsNull:bool; EqNull:bool; IsValue:bool; IsNullable:bool; IsOption:string}
[
    "true",box true
    "Nullable", box <| Nullable()
    "N true",box <| Nullable true
    "Some true",box <| Some true
    "None", box None
    "null", box null
]
|> Seq.map(fun (n,v) ->
    {   Name=n
        IsNull=isNull v
        EqNull =  Object.ReferenceEquals(null,v)
        IsValue = isV v
        IsNullable = isN v
        IsOption=isOpt v
    }
)
|> Dump
|> ignore