<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Compiler.Service</NuGetReference>
</Query>

// adapted from https://blog.codinghorror.com/creating-even-more-exceptional-exceptions/
let blacklist = ["Hyperlinq";"Resources";"LINQPad";"query_"]
let debug = true
// example showing how to search a specific assembly
let assemblies = [typeof<Microsoft.FSharp.Compiler.FSharpErrorInfo>.Assembly] @ (System.AppDomain.CurrentDomain.GetAssemblies() |> List.ofSeq)


type TypeReflectionInfo = {Namespace:string; Cases:Reflection.UnionCaseInfo[]; Type:Type}
type ReflectionSearchResult = { AssemblyName:string; Items:TypeReflectionInfo list option; Assembly:Assembly }
type SearchInput = Assembly*TypeInfo seq option

module Helpers = 
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
        //t.Namespace, extraInfo,t
        {Namespace=t.Namespace; Cases= extraInfo; Type=t}
open Helpers
module Searches = 
    let reflectionSearch pattern (definedTypes:TypeInfo seq) =    
        definedTypes
        |> Seq.filter(fun dt -> Regex.IsMatch(dt.Name,pattern)) 
        |> Seq.map mapDefinedType
        |> List.ofSeq 
    
    let findByT f (x:TypeInfo seq) : TypeReflectionInfo list = 
        x
        |> Seq.filter f
        |> Seq.map mapDefinedType
        |> List.ofSeq
    // yes the x is necessary if the method isn't currently being called when compiled
    let findValueTypes x = findByT (fun ti -> ti.IsValueType) x
    let findPrimitiveTypes x = findByT (fun ti -> ti.IsPrimitive) x
    let findPrimitiveNonValue x = findByT (fun ti -> ti.IsPrimitive && not ti.IsValueType) x

    
open Searches

type SearchType = 
    | Name of regex:string
    | Any of (TypeInfo seq -> TypeReflectionInfo list)
    
let strategize strategy = 
    match strategy with
    | Name regex -> reflectionSearch regex
    | Any f -> f
    
let conductStripSearch x = 
    assemblies
    |> Seq.filter(fun a -> blacklist |> Seq.forall (fun bl -> not <| a.FullName.StartsWith bl))
    |> Seq.map(fun a-> a,tryGetDefinedTypes a)
    |> Seq.map(fun (a,definedTypesOpt) -> { AssemblyName= a.FullName
                                            Assembly=a
                                            Items= definedTypesOpt |> Option.map (strategize x)
                                            }
    )
        
//|> reflectionSearch ".*(SynModuleOrNamespace).*"
//conductStripSearch (Name ".*(SynModuleOrNamespace).*")
conductStripSearch (Any Searches.findPrimitiveNonValue)
|> fun items -> 
    items 
    |> Seq.sumBy( fun x -> match x.Items with Some dts -> dts |> Seq.length | _ -> -1)
    |> printfn "found %i"
    items
|> Dump
|> ignore