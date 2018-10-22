<Query Kind="FSharpExpression" />

// sieve
let sieve max = 
//    let memLimit = 200000 //Int32.MaxValue - 100000
    let memLimit = max
    let x = Array.create (memLimit+1) true
    
    [3..memLimit]
    |> Seq.filter(fun i -> i * i <= memLimit)
    |> Seq.iter(fun i ->
        if not x.[i] then
            ()
        else
            // count by i skipping i
            [i .. i .. memLimit]
            |> Seq.skip 1
            |> Seq.iter(fun nonPrime ->
                try
                    x.[nonPrime] <- false
                with _ ->
                    printfn "failing on %i" nonPrime
                    reraise()
            )
    )
    x
    |> Seq.mapi(fun i x -> (i,x))
    |> Seq.filter snd
    |> Seq.map fst
    |> List.ofSeq
let items = sieve 299999
items
|> Seq.take 1000