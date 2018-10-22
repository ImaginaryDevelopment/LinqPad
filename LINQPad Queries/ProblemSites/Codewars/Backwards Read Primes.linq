<Query Kind="FSharpProgram" />

// sieve
// needs optimization, times out, 17.865 seconds to run

// Create sieve
let initSieve topCandidate =
    let result = Array.zeroCreate<bool> (topCandidate + 1)
    Array.set result 2 true
    Array.set result 3 true
    Array.set result 5 true
    result
// Remove squares of primes
let removeSquares sieve topCandidate =
    let squares =
        seq { 7 .. topCandidate}
            |> Seq.filter (fun n -> Array.get sieve n)
            |> Seq.map (fun n -> n * n)
            |> Seq.takeWhile (fun n -> n <= topCandidate)
    for n2 in squares do
        n2
            |> Seq.unfold (fun state -> Some(state, state + n2))
            |> Seq.takeWhile (fun x -> x <= topCandidate)
            |> Seq.iter (fun x -> Array.set sieve x false)
    sieve

// Flip solutions of the first equation
let doFirst sieve topCandidate =
    let set1 = Set.ofList [1; 13; 17; 29; 37; 41; 49; 53]
    let mutable x = 1
    let mutable y = 1
    let mutable go = true
    let mutable x2 = 4 * x * x
    while go do
        let n = x2 + y*y
        if n <= topCandidate then
            if Set.contains (n % 60) set1 then
                Array.get sieve n |> not |> Array.set sieve n

            y <- y + 2
        else
            y <- 1
            x <- x + 1
            x2 <- 4 * x * x
            if topCandidate < x2 + 1 then
                go <- false
// Flip solutions of the second equation
let doSecond sieve topCandidate =
    let set2 = Set.ofList [7; 19; 31; 43]
    let mutable x = 1
    let mutable y = 2
    let mutable go = true
    let mutable x2 = 3 * x * x
    while go do
        let n = x2 + y*y
        if n <= topCandidate then
            if Set.contains (n % 60) set2 then
                Array.get sieve n |> not |> Array.set sieve n

            y <- y + 2
        else
            y <- 2
            x <- x + 2
            x2 <- 3 * x * x
            if topCandidate < x2 + 4 then
                go <- false
// Flip solutions of the third equation
let doThird sieve topCandidate =
    let set3 = Set.ofList [11; 23; 47; 59]
    let mutable x = 2
    let mutable y = x - 1
    let mutable go = true
    let mutable x2 = 3 * x * x
    while go do
        let n = x2 - y*y
        if n <= topCandidate && 0 < y then
            if Set.contains (n % 60) set3 then
                Array.get sieve n |> not |> Array.set sieve n

            y <- y - 2
        else
            x <- x + 1
            y <- x - 1
            x2 <- 3 * x * x
            if topCandidate < x2 - y*y then
                go <- false

// Pick the primes and return as an Array
let pickPrimes sieve =
    sieve
        |> Array.mapi (fun i t -> if t then Some (i,()) else None)
        |> Array.choose (fun t -> t)
        |> Map.ofArray
//    |> Seq.mapi(fun i x -> if x then Some i else None)
//    |> Seq.choose id
//    |> Set.ofSeq
    
// Sieve of Atkin
let ListAtkin (topCandidate : int) =
    let sieve = initSieve topCandidate

    [async { doFirst sieve topCandidate }
     async { doSecond sieve topCandidate }
     async { doThird sieve topCandidate }]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore

    removeSquares sieve topCandidate |> pickPrimes
let inline reverseDigit x=
  let mutable x,acc=x,0
  while x<>0 do 
    acc<-acc*10+x%10
    x<-x/10
  acc
let backwardsContains target (values:int[]) =
    seq{values.Length - 1 .. -1 .. 2}
    |> Seq.map(fun i -> values.[i])
    |> Seq.skipWhile(fun prime -> target + 1 < prime)
    |> Seq.takeWhile(fun prime -> target - 1 < prime)
    |> Seq.contains target
    
let inline backwardsPrime (markers:Map<int,_>) (min:int64) (max:int64) =
    let min,max = int min, int max
    
    markers
    |> Map.toSeq
    |> Seq.map fst
    |> Seq.skipWhile(fun x -> x < min)
    |> Seq.takeWhile(fun x -> x <= max)
//    |> Seq.filter(fun x -> min <= x && x <= max)
    |> Seq.filter(fun x ->
        let backwards = reverseDigit x
//        x <> backwards && backwardsContains backwards markers
        x <> backwards && Map.containsKey backwards markers
    )
    |> Seq.map int64
    |> List.ofSeq
let sw = System.Diagnostics.Stopwatch()
sw.Start()
let markers = ListAtkin 130000007
printfn "Done getting primes at %A" sw.Elapsed
sw.Stop()
backwardsPrime markers 1095000L 1095403L
|> Dump
|> ignore