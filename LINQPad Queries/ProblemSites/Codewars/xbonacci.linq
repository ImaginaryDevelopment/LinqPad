<Query Kind="FSharpExpression" />

// xbonacci

let bonacc (prevX:_ list) totalValues =
    let lastX = prevX |> Seq.toArray
    // where was the last value stored, so we don't have to shift/dequeue an array
    let lastXI = lastX.Length
    if totalValues <= lastXI then
        prevX.[0.. totalValues - 1]
        |> Seq.ofList
    else
        seq{
            yield! lastX
            for i in lastXI .. totalValues  do
                let cur = lastX |> Array.sum
                yield cur
                lastX.[i % lastX.Length] <- cur
        }
    
    
//bonacc [1;1] 10
let fibonacci = bonacc [1;1]
let tribonacci = bonacc [0;1;1]


//fibonacci 10, tribonacci 10

bonacc [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] 9