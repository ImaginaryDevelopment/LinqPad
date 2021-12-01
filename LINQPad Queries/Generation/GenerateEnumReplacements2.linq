<Query Kind="FSharpProgram" />

let pascal (x:string) = string (Char.ToUpper(x.[0])) + x.[1..]
let types =
    typeof<Enums>.GetMembers()
    |> Seq.choose(
            function
            | :? System.Type as t when t.IsEnum -> Some t
            | _ -> None
            )
        
let writeEnum (t:Type) =
    let bodyIndent = "             "
    let buildExplicit tt items =
        [
            yield sprintf "        static member op_Explicit source : %s =" tt
            yield sprintf "%smatch source with" bodyIndent
            yield! items |> Seq.map (sprintf "%s%s" bodyIndent)
        ]
        |> String.concat "\r\n"
    t
    |> FSharp.Reflection.FSharpType.GetUnionCases
    |> fun x ->
        let values = x |> Array.map(fun e -> sprintf "| %A -> %s.%s" e.Tag e.DeclaringType.Name e.Name)// |> String.concat "\r\n"
        let values2 = x |> Array.map(fun e -> sprintf "| %s.%s -> %A" e.DeclaringType.Name e.Name e.Tag)// |> String.concat "\r\n"
        let t = x.[0].DeclaringType.Name
        [
            yield "    with"
            yield buildExplicit "int" values2
            yield buildExplicit t values
            yield sprintf "%s| x -> failwithf \"Value %%i not found in %s\" x" bodyIndent t
        ]
        |>  String.concat "\r\n"
()
types
|> Seq.map(fun t ->
    
    let isFlags = t.GetCustomAttribute<FlagsAttribute>() |> isNull |> not
    let ns = Enum.GetNames t
    let vs = Enum.GetValues t |> Seq.cast<int> |> List.ofSeq
    let both = Seq.zip ns vs
    if Seq.length ns <> Seq.length vs then failwithf "Bad call %s" t.Name
    [
        if isFlags then 
            yield "[<Flags>]"
        yield sprintf "type %s =" t.Name
        yield! both |> Seq.map(fun (x,y) -> sprintf "    | %s = %i" x y )
    ]
    |> String.concat "\r\n"
)
|> Seq.append [
    yield "namespace HeroDesigner.Schema"
    yield ""
    yield "open System"
    yield ""
    yield! types |> Seq.map(fun t -> sprintf "[assembly: TypeForwardedTo(typeof(HeroDesigner.Schema.%s))]" t.Name)
]
|> String.concat "\r\n"
|> Dump
|> ignore