<Query Kind="FSharpProgram" />

let target = "G:\steamlibrary/steamapps/common"
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
        
let rec getSize x =
    [
        Directory.GetFiles x
        |> Seq.map (FileInfo >> fun x -> x.Length)
        |> Seq.sum
    ]
    |> Seq.sum
Directory.GetDirectories target
|> Seq.map (fun x -> Path.GetFileName x, getSize x,getSize x |> decimal |> cleanDisplay Bytes)
|> Seq.sortBy (fun (_,sz,_) -> -1L * sz)
|> Seq.map (fun (n,_,disp) -> n, sprintf "%A" (fst disp))
|> Dump
|> ignore