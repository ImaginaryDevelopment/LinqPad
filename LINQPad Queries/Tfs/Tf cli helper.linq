<Query Kind="FSharpProgram" />

// translate local path to source control explorer path
let tfPath = @"C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\Common7\IDE\CommonExtensions\Microsoft\TeamFoundation\Team Explorer\TF.exe"
let targetPath = Util.ReadLine("Path?")
module Helpers =
    let (|RMatch|_|) p x =
        let m = Regex.Match(x,p)
        if m.Success then Some m
        else None
    let after (d:string) (x:string) =
        x.[x.IndexOf d + d.Length ..]
    let split (d:string) : string -> string[] = function | null | "" -> Array.empty | x -> x.Split([| d |], StringSplitOptions.None)
    let delimit (d:string) (x:string seq) = String.Join(d,x)
    let trim = function | null | "" -> null | x -> x.Trim()
open Helpers
module Tf =
    type WorkFold = {Workspace:string;Collection:string; SourcePath:string;FilePath:string}
    let tf quiet args =
        Util.Cmd(tfPath,args,quiet)
    let add p =
        let _result = 
            let args = sprintf "add \"%s\"" p
            tf true args
        ()
    let workfold p = 
        let args = sprintf "vc workfold \"%s\"" p
        let lines = tf true args
        let findHeader d (x:string seq) = x|> Seq.tryFind(fun x -> not <| isNull x && x.StartsWith d) |> Option.map(after d)
        let workspace = findHeader "Workspace : " lines
        // yes the header spacing is inconsistent
        let coll = findHeader "Collection: " lines
        let pathLine = if lines.Length > 2 then lines.[3] |> split ":" else Array.empty
        let sourcePath = if pathLine.Length > 0 then pathLine.[0] |> trim else ""
        let filePath = if pathLine.Length > 1 then pathLine.[1..] |> delimit ":" |> trim else null
        let x = {Workspace=defaultArg workspace "";Collection = defaultArg coll "";SourcePath = sourcePath;FilePath=filePath}
        x, lines
        
    
    let status p =
        let _result = 
            let args = sprintf "status /format:detailed \"%s\"" p
            tf false args
        ()
    let help () =
        tf false "vc help"
        |> ignore
        
open Tf

workfold targetPath //(Util.ReadLine("Path?"))
|> Dump
|> ignore