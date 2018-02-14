<Query Kind="FSharpProgram" />

// maintain all versions of breusable in a single repo
module Helpers =
    type String with
        static member split (d:string) (x:string) =
            x.Split([| d|], StringSplitOptions.RemoveEmptyEntries)
        static member trim (x:string) = x.Trim()
    let (|RMatch|_|) (p:string) (x:string) = 
        let r = Regex.Match(x,p)
        if r.Success then Some r
        else None
    // extend/hack the LINQPad.ObjectGraph.RawHtml deal
    module RawHtml = 
        let private t = Util.RawHtml("").GetType()
        let private createRaw = 
            let cons = 
                t.GetConstructor([| typeof<string>|])
            fun (s:string) -> 
                cons.Invoke([| s|])
               
        let getContent =
            let m =
                t.GetMethod("GetContent")
    //        m.Dump("getContent")
            fun (s:obj) ->
                let sType = s.GetType()
                match s with
                | x when x.GetType() = t ->
                    m.Invoke(x, [| |])
                    
                | _ -> failwithf "Expected %s but was %s" t.FullName sType.FullName
        let wrap start finish (middle:obj) =
            let c = getContent middle :?> String
            sprintf "%s%s%s" start c finish
            |> createRaw

open Helpers
let target =  @"C:\projects\FsInteractive"
let versions =
    Environment.GetEnvironmentVariable("breusables")
    |> String.split ";"
    |> Seq.map String.trim
    |> Seq.filter (fun x -> x.StartsWith target |> not)
    
let map = 
    function
    | RMatch "(\w+)\.Schema" r -> r.Groups.[1].Captures.[0].Value
    | _ -> failwithf" No Map found"
    
let toCopy = 
    versions
    |> Seq.map (fun v -> (v, Path.GetFileName v, map v))
    |> Seq.map (fun (src, srcFN, targetPrefix) -> src, Path.Combine(target, sprintf "%s%s" targetPrefix srcFN))
    |> List.ofSeq
    
toCopy
|> List.map (fun (src, t) ->
    File.Copy(src,t, true)
    sprintf "Copied from %-60s to %s" src t
)
|> Util.ToHtmlString
|> Util.RawHtml
|> RawHtml.wrap"<div style='font-family:monospace'>" "</div>"
|> Dump
|> ignore
    