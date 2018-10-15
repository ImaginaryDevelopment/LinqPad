<Query Kind="FSharpProgram" />

let findNb(m: uint64): int =
    let getVolume i =
        pown i 3
    Seq.initInfinite(fun i -> uint64 i |> getVolume)
    |> Seq.skip 1
    |> Seq.scan(fun (i,v) nv ->
        (i+1,v+nv)
    ) (0,0UL)
    |> Seq.skip 1
        
    |> Seq.takeWhile(fun (_,v) -> v <= m)
//    |> Seq.last
    |> Seq.tryFind(fun (_,v) -> v = m)
    |> function
        |Some (i,_) -> i
        |None -> -1
    
findNb 1071225UL
//     
|> Dump
|> ignore
