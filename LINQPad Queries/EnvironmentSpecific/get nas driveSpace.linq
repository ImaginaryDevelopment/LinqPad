<Query Kind="FSharpProgram">
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

// disk space explorer

// this script was made possible in part thanks to :
// https://superuser.com/questions/1124677/access-to-network-share-failed-after-latest-windows-10-cumulative-patch-kb318986/1127403#1127403
// http://stackoverflow.com/a/17988848/57883
[<AutoOpen>]
module PInvoke =
    type long = Int64
    [<DllImport("kernel32.dll")>]
    extern int private GetDiskFreeSpaceEx(string lpDirectoryName,long& lpFreeBytesAvailable, long& lpTotalNumberOfBytes, long& lpTotalNumberOfFreeBytes);
    type DiskSpaceResult = { FreeBytesAvailable: long; TotalNumberOfBytes: long; TotalNumberOfFreeBytes: long}
    let getDiskFreeSpace dirName =
        let mutable a = 0L
        let mutable t = 0L
        let mutable f = 0L
        let result = GetDiskFreeSpaceEx(dirName, &a, &t, &f)
        {FreeBytesAvailable=a; TotalNumberOfBytes=t; TotalNumberOfFreeBytes= f}, result
// uppercase B is bytes, b is bits, iirc
//[<Measure>] type GB
//[<Measure>] type B
//[<Measure>] type MB

type ByteMeasure = 
    | Bytes
    | KB // proper casing for measure is kB ?
    | MB
    | GB
type DiskSpaceDisplay = { FreeBytes: string; TotalBytes: string; TotalFreeBytes: string}
let rec cleanDisplay m (bytes:decimal) = 
    // yay literals!
    //let pie = 0.31415e1 |> Dump
    let nextOpt =
        match m with
        | Bytes -> Some KB
        | KB -> Some MB
        | MB -> Some GB
        | _ -> None
    let gap = 1e3m
    match nextOpt, bytes > gap with
    | Some next, true ->
        Math.Round(bytes / gap, 2) 
        |> cleanDisplay next 
    | _ -> 
        let display:string = sprintf "%M%A" (Math.Round(bytes,if bytes > 100m then 0 else 2)) m
        display,(m,bytes)
        

//type SpacePath = 
//    | NetworkShare
//    | LocalDrive
let path = 
    // local path also works
    //@"C:\"
    // without share it did nothing    
    let server, share = "fs01", "builds"
    sprintf @"\\%s\%s" server share
let cleanBytes = decimal >> cleanDisplay Bytes >> fst
getDiskFreeSpace path
|> Dump
|> fst
|> fun x -> {FreeBytes=cleanBytes x.FreeBytesAvailable; TotalBytes=cleanBytes x.TotalNumberOfBytes; TotalFreeBytes = cleanBytes x.TotalNumberOfFreeBytes}
|> Dump
|> ignore