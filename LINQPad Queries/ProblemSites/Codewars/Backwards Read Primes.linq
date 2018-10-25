<Query Kind="FSharpProgram" />

let isPrime (n:bigint) =
    let maxFactor = bigint(sqrt(float n))
    let rec loop testPrime tog =
        if testPrime > maxFactor then true
        elif n % testPrime = 0I then false
        else loop (testPrime + tog) (6I - tog)
    if n = 2I || n = 3I || n = 5I then true
    elif n <= 1I || n % 2I = 0I || n % 3I = 0I || n % 5I = 0I then false
    else loop 7I 4I

let inline reverseDigit x=
  let mutable x,acc=x,0L
  while x<>0L do 
    acc<-acc*10L+x%10L
    x<-x/10L
  acc
    
let inline getBackwardsPrimes (min:int64) (max:int64) =
    if min < 1L || max < 1L then List.empty
    else 
        printfn "Checking %A..%A" min max
        seq{min .. max}
        |> Seq.filter(fun x ->
            try
                let x' = bigint x
                isPrime x'
            with ex ->
                printfn "Failing on %i" x
                reraise()
        )
        |> Seq.filter(fun x ->
            let backwards = reverseDigit x
            x <> backwards && bigint backwards |> isPrime
        )
        |> Seq.map int64
        |> List.ofSeq


let inline backwardsPrime m n = getBackwardsPrimes m n
getBackwardsPrimes 70000L 70245L
|> Dump
|> ignore