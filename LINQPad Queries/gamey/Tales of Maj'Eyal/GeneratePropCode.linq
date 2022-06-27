<Query Kind="FSharpProgram">
  <Reference>D:\projects\ToMEHelper\src\ToMEHelper\bin\Debug\net5.0\ToMEHelper.dll</Reference>
</Query>

// take a type, that is a Partial<'t>
// make helpers for generating testers for related DU values
open System.Reflection
let genFieldDU name indent propNames =
    [
        yield "[<RequireQualifiedAccess>]"
        yield sprintf "type %s =" name
        yield! propNames
                |> Seq.map(sprintf "%s| %s" indent)
        yield sprintf "%swith" indent
        yield sprintf "%s%sstatic member All = [" indent indent
        yield! propNames
            |> Seq.map(sprintf "%s%s%s%s.%s" indent indent indent name)
        yield sprintf "%s%s]" indent indent
    ]
// assume record
let genEmpty (props : PropertyInfo seq) =
    props
    |> Seq.map(fun x ->
        let genText = sprintf "%s = %s" x.Name
        // how do we tell if it is an option?
        if x.PropertyType.Name.StartsWith "FSharpOption`1" then
            genText "None"
        elif x.PropertyType.Name.StartsWith "FSharpList`1" then
            genText "List.empty"
        else x.Name
            
    )
    
let genFieldMapFn fieldDUName propNames =
    [
        yield "function"
        yield! propNames
                |> Seq.map(sprintf "| %s.%s -> \"\"" fieldDUName)
    ]

let generateMetaProps indent fieldDUName tName propNames =
    [
        yield ["module Meta ="]
        yield [ sprintf "%stype Prop<'t> = %s * (%s -> 't) * ('t -> %s -> %s)" indent fieldDUName tName tName tName]
        yield! propNames
                |> Seq.map(fun n ->[
                    sprintf "%slet %sProp: Prop<_> = " indent n
                    sprintf "%s%s%s.%s, (fun x -> x.%s), fun y x -> {x with %s = y}" indent indent fieldDUName n n n
                ])
    ]
    |> Seq.collect id
// filter type + fieldField value -> (string * string) option
// assume .All works
let generateMapYield indent tName fieldDUName = [
    yield sprintf "let toStringMap (fGet: %s -> %s -> string option) (filter:%s) =" fieldDUName tName tName 
    yield sprintf "%s%s.All |> Seq.choose (fun f -> fGet f filter)" indent fieldDUName
]
let t = typeof<ToMEHelper.Scraping.ApiHelpers.CharacterFilter>
let props = t.GetProperties(BindingFlags.Instance ||| BindingFlags.Public)
let fieldDUName = "CharacterFilterField"
let indent = "    "
props |> genEmpty |> String.concat "\r\n" |> Dump |> ignore
printfn ""
props |> Seq.map(fun x -> x.Name) |> genFieldDU fieldDUName indent |> String.concat "\r\n" |> Dump |> ignore
printfn ""
props |> Seq.map(fun x -> x.Name) |> genFieldMapFn fieldDUName |> String.concat "\r\n" |> Dump |> ignore
printfn ""
props |> Seq.map(fun x -> x.Name) |> generateMetaProps indent fieldDUName t.Name |> String.concat "\r\n" |> Dump |> ignore
printfn ""
generateMapYield indent t.Name fieldDUName |> Dump |> ignore

//|> Seq.map(fun p -> p.Name)

