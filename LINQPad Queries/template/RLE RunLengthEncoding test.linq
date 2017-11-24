<Query Kind="FSharpProgram" />


let debug = true
// placeholder
let runLengthEncode (input:byte[]) = input

let uncurry f (a, b) = f a b

let compareBytes = Seq.compareWith (fun a b ->
    if a > b then 1
    elif a < b then -1
    else 0)	
    
let test a b : bool =
    try
        let actual=a |> Array.ofList |> runLengthEncode |> List.ofArray
        let seq=compareBytes actual b
        if debug then
            Console.Write "\tresult:"
            Console.WriteLine seq
        seq = 0
    with
        | ex -> 
        
            Console.WriteLine("fail!" + ex.Message)
            false
let main ()= 
    [
        [ 1uy;1uy;1uy;2uy;2uy;2uy], [3uy;1uy;3uy;2uy]
        [ 0uy; 1uy; 2uy; 3uy],[1uy; 0uy; 1uy; 1uy; 1uy; 2uy; 1uy; 3uy]
        List.empty, List.empty
    ]
    |> Seq.map(uncurry test)
    
main()
|> Dump
|> ignore