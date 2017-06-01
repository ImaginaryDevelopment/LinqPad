<Query Kind="FSharpExpression">
  <NuGetReference>FSharp.Compiler.Service</NuGetReference>
</Query>

// adapted from https://blog.codinghorror.com/creating-even-more-exceptional-exceptions/
let blacklist = ["Hyperlinq";"Resources";"LINQPad";"query_"]
let debug = true

let tryGetDefinedTypes (a: Assembly) = 
    try 
        Some a.DefinedTypes
    with ex -> 
        ex.Dump(sprintf "Could get defined types from %s" a.FullName)
        None
let mapDefinedType (t:Type) = 
    let extraInfo = 
        if Reflection.FSharpType.IsUnion t then 
            let cases = Reflection.FSharpType.GetUnionCases(t)
//            let pi = Reflection.FSharpType.GetRecordFields(t)
            cases 
        else null
    extraInfo,t
let reflectionSearch pattern (assemblies:#seq<Assembly>)= 
    assemblies
    |> Seq.map (fun a -> 
        
        a.FullName, tryGetDefinedTypes a |> Option.map (Seq.filter(fun dt -> Regex.IsMatch(dt.Name,pattern)) >> Seq.map mapDefinedType), a
    )
    
// example showing how to search a specific assembly
let assemblies = [typeof<Microsoft.FSharp.Compiler.FSharpErrorInfo>.Assembly] @ (System.AppDomain.CurrentDomain.GetAssemblies() |> List.ofSeq)
//type DUTest = 
//    |Foo
//    |Bar
//Microsoft.FSharp.Reflection.FSharpType.IsUnion(typeof<DUTest>).Dump()    
//typeof<DUTest>.Dump()
assemblies
|> Seq.filter(fun a -> blacklist |> Seq.forall (fun bl -> not <| a.FullName.StartsWith(bl)))
|> reflectionSearch ".*(NestedModule).*"
|> fun items -> (items |> Seq.sumBy( fun (fn, dt,a) -> match dt with Some dts -> dts |> Seq.length | _ -> -1), items)
//|> Dump
//|> ignore