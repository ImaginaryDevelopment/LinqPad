<Query Kind="FSharpProgram" />


//let validatePath p =
//    match Path.IsPathRooted p with
//    | true -> true
//    | false -> 
//        // root it and check that?
//        Environment.GetLogicalDrives() 
//        |> Seq.head
//        |> fun r -> Path.Combine(r,p)
//type PathType =
//    |Absolute of string
//    |Relative of string
//    |Uri of Uri
let (|StartsWith|_|) (d:string) (x:string) = if x.StartsWith(d) then Some () else None
let tryf f = 
    try
        f() |> Choice1Of2
    with ex -> Choice2Of2 ex

type TryBoolResult = 
    | Result of bool
    | Exception of exn
let toBoolResult x =
    match x with
    | Choice1Of2 b -> Result b
    | Choice2Of2 ex -> Exception ex

    
type UriResult = { IsWellFormedOriginalString:TryBoolResult; LocalPath:string; IsUnc: bool} // :Choice<string,exn>
type UriDetectorResult =
    | UriFailed of exn
    | Created of UriResult
let getUriDetectorResult p = 
    match tryf (fun () -> Uri p) with
        | Choice1Of2 uri -> 
            {
                IsWellFormedOriginalString = tryf (fun () -> uri.IsWellFormedOriginalString()) |> toBoolResult
                LocalPath = uri.LocalPath // tryf (fun () -> uri.LocalPath)
                IsUnc= uri.IsUnc
            }
            |> Created
        | Choice2Of2 ex -> UriFailed ex
        
type PathDetectorResult = {IsRooted:TryBoolResult;DirExists:TryBoolResult;FileExists:TryBoolResult; HasInvalidPathChars:bool;HasInvalidFileNameChars:bool }
//type ValidPathConfidence = 
//    |Exists
//    |Maybe of PathDetectorResult

let getPathDetectionResults p = 
    let tryfPath f = tryf (fun () -> f p)
    p,getUriDetectorResult p,
        { 
            IsRooted = tryfPath Path.IsPathRooted |> toBoolResult
            // these two should only ever be false or exn in this path
            DirExists= tryfPath Directory.Exists |> toBoolResult
            FileExists = tryfPath File.Exists |> toBoolResult
            HasInvalidPathChars= Path.GetInvalidPathChars() |> Seq.map string |> Seq.exists p.Contains
            HasInvalidFileNameChars = Path.GetInvalidFileNameChars() |> Seq.map string |> Seq.exists p.Contains
        }

let pathDetectionToPretty (p,uriD, pd): string*string*string =
    let inline sprintStrip x = x |> (sprintf "%A" >> replace "\r" String.Empty >> replace "\n" String.Empty)
    p, sprintStrip uriD, sprintStrip pd
Environment.CurrentDirectory <- @"C:\"
[
    @"C:\\tfs\\share"
    @"C:\\tfs\share"
    @"C:\tfs\\share\"
    @"C:\tfs\\share\\"
    @"file://c:/tfs/share"
    @"tfs/share"
    @"\\localhost\share"
]
|> Seq.map (getPathDetectionResults >> pathDetectionToPretty )
    

//    match d with
//    | StartsWith "file://" -> uri,uri.IsWellFormedOriginalString(), false, (uri.IsUnc |> string) uri.GetComponents
//    | _ -> uri,uri.IsWellFormedOriginalString(), Directory.Exists d, Path.GetFullPath d
//)

|> Dump
|> ignore
// are we sure?"
//let validPaths = 
//    [ @"c:\\directory\filename" ]
//Uri()
