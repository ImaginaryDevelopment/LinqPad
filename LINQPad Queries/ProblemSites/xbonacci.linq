<Query Kind="FSharpExpression" />

// xbonacci

let bonacc prevX totalValues =
    let lastX = prevX |> Seq.toArray
    // where was the last value stored, so we don't have to shift/dequeue an array
    let lastXI = lastX.Length
    seq{
        yield! lastX
        for i in lastXI .. totalValues - 1 do
            let cur = lastX |> Array.sum
            yield cur
            lastX.[i % lastX.Length] <- cur
    }
    
    
//bonacc [1;1] 10
let fibonacci = bonacc [1;1]
let tribonacci = bonacc [0;1;1]


fibonacci 10, tribonacci 10
    