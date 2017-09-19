<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Core</NuGetReference>
</Query>

let workDir = @"C:\builds\work"
//if not <| Directory.Exists workDir then 
//    failwith "builds\work directory does not exist"
printfn "LinqPad Script Current Directory: %s" Environment.CurrentDirectory
module Helpers =
    let delimit (d:string) x = 
        let v = x |> Array.ofSeq
        String.Join(d,value= v)
    let indexOf (delimiter:string) (s:string) :int = 
        s.IndexOf delimiter
    let lastIndexOf (delimiter:string) (s:string) :int =
        s.LastIndexOf delimiter
    let trim (s:string) = s.Trim()
    let split (delimiters: string[]) (options:StringSplitOptions) (s:string) = 
        s.Split(delimiters,options)
    
    let private b4 f (s:string) = 
        s.Substring(0,f s)
    let private aft f (s:string) = 
        s.Substring(f s)
        
    let before (delimiter:string) (s:string) :string =
        b4 <| indexOf delimiter <| s
        
    let after (delimiter:string) (s:string) :string = 
        indexOf delimiter >> (+) delimiter.Length
        |> aft <| s
    
    let beforeLast (delimiter:string) (s:string) :string = 
        if s.Contains delimiter then 
            b4 <| lastIndexOf delimiter <| s
        else s
        
    let afterLast delimiter (s:string) = 
        lastIndexOf delimiter >> (+) delimiter.Length
        |> aft <| s
open Helpers

let (|Number|_|) (x:string) = 
    match System.Int32.TryParse x with
    | true, _ -> Some()
    | _ -> None
let (|RMatch|_|) (p:string) (x:string) = 
    let r = Regex.Match(x,p)
    if r.Success then
        Some r
    else None

let formatBytes (x:System.Int64) = 
    let k = 1000L
    let mb = k * 1000L
    let gb = mb * 1000L
    if x >= gb then
        float x / float gb |> sprintf "%.2f GB"
    elif x >= mb then
        float x / float mb |> sprintf "%.2f MB"
    elif x >= k then
        float x / float k |> sprintf "%.2f KB"
    else sprintf "%d2" x
    
let getFreeSpace () = 
    try
        Util.Cmd("fsutil volume diskfree c:", true) 
        //|> printfn "%A"
        |> Seq.choose(fun line ->
            match line with
            | RMatch "\d+" r ->
                //let formatted = String.Format("{0:n0}",Int64.Parse r.Value)
                let formatted = Int64.Parse r.Value |> formatBytes
                sprintf "%s%s" (line |> before r.Value) formatted
                
                |> Some
            | _ -> None)
        |> delimit "\r\n"
    with ex -> 
        Console.Error.WriteLine(sprintf "%A" ex)
        ""
getFreeSpace() 
|> printfn "%A"

let targets = 
    Directory.GetDirectories workDir
    |> Seq.filter(afterLast "\\" >> function | Number -> true | _ -> false)
Environment.CurrentDirectory <- workDir
targets
|> Dump
|> Seq.iter(fun t ->
    printfn "removing subdirectories of %s" t
    let cmdLine = sprintf "rmdir /s /q \"%s\"" t
    try
        Util.Cmd(cmdLine,false) |> ignore
    with ex -> 
        Console.Error.WriteLine(sprintf "Failed: '%s'" cmdLine)
        Console.Error.WriteLine(sprintf "%A" ex)
)
getFreeSpace()
|> printfn "%A"
