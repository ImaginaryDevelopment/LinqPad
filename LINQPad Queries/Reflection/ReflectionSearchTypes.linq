<Query Kind="FSharpExpression" />

// adapted from https://blog.codinghorror.com/creating-even-more-exceptional-exceptions/
let blacklist = ["Hyperlinq";"Resources";"LINQPad";"query_"]


let tryGetDefinedTypes (a: Assembly) = 
    try 
        Some a.DefinedTypes
    with ex -> None
    
let reflectionSearch pattern (assemblies:#seq<Assembly>)= 
    assemblies
    |> Seq.map (fun a-> a.FullName, tryGetDefinedTypes a |> Option.map (Seq.filter(fun dt -> Regex.IsMatch(dt.Name,pattern))), a)
    
    
let assemblies = System.AppDomain.CurrentDomain.GetAssemblies()


assemblies
|> Seq.filter(fun a -> blacklist |> Seq.forall (fun bl -> not <| a.FullName.StartsWith(bl)))
|> reflectionSearch ".*([Ll]en|[mM]ax).*Attribute"
|> fun items -> (items |> Seq.sumBy( fun (fn, dt,a) -> match dt with Some dts -> dts |> Seq.length | _ -> -1), items)

