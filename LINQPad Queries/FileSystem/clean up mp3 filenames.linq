<Query Kind="FSharpProgram" />

let target = @"G:\takeout-20201119T182229Z-001\Takeout\Google Play Music\Tracks"
type OriginalName = Name of string
let flip f x y = f y x
let startsWith (delim:string) (x:string) =
    x.StartsWith delim
let endsWith (delim:string) (x:string) =
    x.EndsWith delim
let startsSpace (x:string) = String.IsNullOrWhiteSpace x.[0..0]
let mapBoth fDetect fChange =
    let change x = fChange x
    function
    | Error x -> if fDetect x then change x |> Error else Error x
    | Ok x -> if fDetect x then change x |> Error else Ok x
    
let trim (x:string) = x.Trim()
let trimStart (delim:string) (x:string) =
    if x.StartsWith(delim) then
        let r = x.[x.IndexOf delim + delim.Length..]
        r
    else x
let trimStartAll (delims:string list) (x:string) =
    let rec tsa (x:string) =
        delims
        |> Seq.tryFind(flip startsWith x)
        |> function
            | Some d ->
                x |> trimStart d |> trim
                |> tsa
            | None when startsSpace x -> trim x |> tsa
            | None -> trim x
    tsa x
let (>||>) f1 f2 x = f1 x || f2 x
module Bads =
    module DupedEnd =
        let isDupeEnd = endsWith ".mp3.mp3"
        let remDupeEnd (fn:string) =
            Path.GetFileNameWithoutExtension fn
           
            
        let fixMp3Dupe =
            mapBoth isDupeEnd remDupeEnd
            
    module BadStart =
        let is = startsWith "-" >||> startsWith " " >||> startsWith " - (000)" >||> startsWith "("
        let fixBad =
            mapBoth is (trimStartAll ["-"; "(000)"])
open Bads    
let fixName (x:string) =
    let on = OriginalName.Name x
    let dir = Path.GetDirectoryName x
    let fn = Path.GetFileName x
    fn
    |> Ok
    |> DupedEnd.fixMp3Dupe
    |> BadStart.fixBad
    |> function
        | Ok x -> Ok <| Path.Combine(dir,x)
        | Error x -> Error (Path.Combine(dir,x),on)
//target
//|> Directory.EnumerateFiles
//|> Seq.filter (Path.GetFileName>>BadStart.is)
//|> Seq.map(fun n ->
//    let fn = Path.GetFileName n
//    fn, fn |> Ok |> BadStart.fixBad, (fn,n) |> Error |> BadStart.fixBad
//)
//|> Dump
//|> ignore
target
|> Directory.EnumerateFiles
|> Seq.map fixName
|> Seq.choose (function | Error x -> Some x | _ -> None)
|> Seq.map(fun (x,Name on) -> File.Move(on,x); (x,on))
|> Dump
|> ignore

