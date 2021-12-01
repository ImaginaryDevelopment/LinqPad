<Query Kind="FSharpProgram" />

// ark make backup

let minSize = 28_000_000
let path = @"D:\Games\steamapps\common\ARK\ShooterGame\Saved"

let files = Directory.EnumerateFiles(path,"*.ark", SearchOption.AllDirectories)
module Split =
    let ofResult items =
        ((List.empty,List.empty), items)
        ||> Seq.fold(fun (oks,es) ->
            function
            | Ok x -> x::oks, es
            | Error e -> oks,e::es
        )
let oks,e =
    files
    |> Seq.map(fun x ->
        try
            let fi = FileInfo(x)
            Ok(x,fi.LastWriteTime, fi.Length)
        with ex ->
            Error(x,ex.Message)
    )
    |> Split.ofResult
let getDeduplicator () =
    DateTime.Now.ToString("yyyy.MM.dd.hh.mm")
let items =
    oks
    |> Seq.sortByDescending(fun (fn,dt,len) -> dt,fn,len)
    |> Seq.filter(fun (_,_,len) -> len > int64 minSize)
    |> List.ofSeq
items
|> Seq.tryHead
|> function 
    | Some (path,dt,_) ->
        let dir, fn = Path.GetDirectoryName path, Path.GetFileName path
        let next = Path.Combine(dir, sprintf "%s.%s.copy" path (getDeduplicator()))
        printfn "Copying %s to %s(%A)" fn next dt
        File.Copy(path,next)
        dir, fn, Path.GetFileName next
    | None -> failwithf "Could not find a qualifying file to backup in '%s'" path 
|> Dump
|> ignore

if not <| Seq.isEmpty e then e.Dump("errors")
items.Dump("considered")
