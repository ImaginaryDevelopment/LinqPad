<Query Kind="FSharpProgram" />

let targets = [@"D:\Games\Steam\steamapps\common" ;"G:\steamlibrary/steamapps/common"]
type ByteMeasure = 
    | Bytes
    | KB // proper casing for measure is kB ?
    | MB
    | GB
    
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
let link v display = LINQPad.Hyperlinq(uriOrPath=v,text=display)        
let rec getSize x =
    [
        yield 
            Directory.GetFiles x
            |> Seq.map (FileInfo >> fun x -> x.Length)
            |> Seq.sum
        yield! Directory.GetDirectories x |> Seq.map getSize
    ]
    |> Seq.sum
let getSizes x = 
    x 
    |> Seq.map (fun x -> link x <| Path.GetFileName x, getSize x,getSize x |> decimal |> cleanDisplay Bytes)
    |> Seq.sortBy (fun (_,sz,_) -> -1L * sz)
let drives = DriveInfo.GetDrives() |> Array.filter(fun x -> x.DriveType = DriveType.Fixed && x.IsReady)

targets
|> Seq.map(fun t -> 
    let root = Path.GetPathRoot t
    let free = drives |> Seq.tryFind(fun x -> x.Name = root) |> Option.map(fun x -> decimal x.AvailableFreeSpace |> cleanDisplay Bytes |> fst)
    link t root,free |> Option.getOrDefault null, Directory.GetDirectories t |> getSizes |> Seq.truncate 20)

|> Seq.map(fun (l,f,x) -> l, f, x |> Seq.map(fun (n,_,(v,_)) -> n,v))
//|> Seq.map (fun (n,_,disp) -> n, sprintf "%A" (fst disp))
|> Dump
|> ignore

drives.Dump("fixed ready drives")