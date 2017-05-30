<Query Kind="FSharpProgram" />

// find functions (in js/ts) that are not mentioned in the test.coffee
// the find part is working, haven't done the match up against test part yet

type Pattern = string
let (|RMatch|_|) (p:Pattern) (s:string) = 
    let m = Regex.Match(s,p)
    if m.Success then
        Some m
    else None
match "hello" with
| RMatch "l+" m -> printfn "Hello!"
| _ -> printfn "boo"
let leadingVarOption = @"^\s*(?:var\s*\w+\s*=\s*)?"
let exportFunction = sprintf @"%sexports\.(\w+)" leadingVarOption
// extension methods for instances
let prototypeFunction = sprintf @"%s(\w+)\.prototype\.(\w+)\s*=" leadingVarOption
// static extension methods for types
let staticFunction = sprintf @"%s([A-Z]\w*)(?:\['|.)(\w+)(?:'\])?\s*=\s*" leadingVarOption
//let lineStarter= @"^\s*(?!//)\s?(?:\w|\()*"
//let fnRegex :Pattern = sprintf @"%s(%s)" lineStarter ([exportFunction;prototypeFunction;staticFunction] |> delimit "|")
let functionNames = 
    let path = @"C:\TFS\PracticeManagement\dev\PracticeManagement\Pm.Web\src\extensions.ts"
    File.ReadAllLines path
    |> Seq.mapi (fun i line -> i,line)
    |> Seq.choose(fun (i,line:string)->
        match line with
        | RMatch prototypeFunction m -> Some ("proto",sprintf "%s.%s"m.Groups.[1].Value m.Groups.[2].Value, m)
        | RMatch exportFunction m -> Some ("export",m.Groups.[1].Value, m)
        | RMatch staticFunction m -> Some ("static",sprintf "%s.%s" m.Groups.[1].Value m.Groups.[2].Value, m)
        | _ -> None
        |> Option.map (fun (t,name,m) ->
            (i,t,name, m.Groups.[0].Value, line)
        )
    )

// guid
// inspect
// getModification
// Object.keys
// String['trim']
// String['contains']
functionNames
|> Dump
|> ignore