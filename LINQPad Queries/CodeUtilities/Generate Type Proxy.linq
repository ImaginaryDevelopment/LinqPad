<Query Kind="FSharpExpression" />

// take a type and generate a proxy for it

let proxyTypeName = "ConsoleWrapper"
let useInheritance = true
let t = typeof<System.IO.TextWriter>

//module Helpers =
let delimit (d:string) x = 
    let v = x |> Array.ofSeq
    String.Join(d,value= v)
let indent i = String.replicate i "    "
let appendLine' x (sb:StringBuilder) = sb.AppendLine x
let appendLine i x = appendLine' <| sprintf "%s%s" (indent i) x
let appendIf b x sb =
    if b then
        appendLine' x sb
    else sb
let getTypeName (t:Type) =
    match t.Name with
    |"Boolean" -> "bool"
//    |"Byte" -> "byte"
//    |"SByte" -> "sbyte"
//    |"Int16" -> "int16"
//    |"UInt16" -> "uint16"
//    |"Int32" -> "int"
//    |"UInt32" -> "uint32"
//    |"Int64" -> "int64"
//    |"Int64" -> "int64"
//    |"Char" -> "char"
//    |"Char[]" -> "char[]"
    |"Object" -> "obj"
    |"Object[]" -> "obj[]"
//    |"String" -> "string"
//    |"Single" -> "float32"
    | x when t.Namespace ="System" -> x.ToLower()
    | x -> x
let generateProperties(t:Type) appendLine =
    let props = t.GetProperties()
    props
    |> Seq.iter (fun p ->
        if p.CanWrite then
            sprintf "override __.%s with get() = wrapped.%s and set v = wrapped.%s <-v" p.Name p.Name p.Name
        else
            sprintf "override __.%s with get() = wrapped.%s" p.Name p.Name
        |> appendLine 0
    )
let generateMethod(m:MethodInfo) isOverloaded =
    let ps =  m.GetParameters()
    let args =
        ps
        |> Seq.map(fun p ->
            p.Name
        )
        |> delimit ","
        |> fun x -> if ps.Length  = 1 && not isOverloaded then x elif ps.Length = 0 then "()" else sprintf "(%s)" x
    let parameters =
        if isOverloaded then
            ps
            |> Seq.map(fun p ->
                sprintf "%s:%s" p.Name <| getTypeName p.ParameterType
            ) |> delimit ","
        elif args.Length = 0 then ""
        else
            ps
            |> Seq.map(fun p ->
                p.Name
            )
            |> delimit ","
    if parameters.Contains("(())") then failwithf "Bad! %s" parameters
            
    let decl =
        if m.IsVirtual && not m.IsFinal then
            sprintf "override"
        // Shadowing if inheritance is used?
        else "member"
    let result = 
        let result = sprintf "%s __.%s(%s) = wrapped.%s %s" decl m.Name parameters m.Name args
//        if m.IsFinal then
//            sprintf "// final:%s" result
//        else result
        result
    if result.Contains("(())") then failwithf "Bad! %s" result
    result
    
    
let generateMethods(t:Type) appendLine =
    t.GetMethods()
    |> Array.filter((fun m -> m.Name.StartsWith "get_" || m.Name.StartsWith "set_" || m.DeclaringType = typeof<obj> || m.IsStatic || (m.IsFinal && useInheritance)) >> not)
    |> Seq.groupBy(fun m -> m.Name)
    |> Seq.iter(fun (_,methods) ->
        methods
        |> Seq.iter(fun m ->
            appendLine 0 <| generateMethod m (Seq.length methods > 1)
        )
    )
let appendf i f sb =
    f (fun j x -> appendLine (i+j) x sb |> ignore<StringBuilder>)
    sb
let generate sb=
    sb
    |> appendLine' (sprintf "type %s(wrapped:%s) =" proxyTypeName t.Name)
    |> appendIf useInheritance (sprintf "%sinherit %s()" (indent 1) t.Name)
    |> appendf 1 (generateProperties t)
    |> appendf 1 (generateMethods t)

let sb = StringBuilder()
generate sb
|> string