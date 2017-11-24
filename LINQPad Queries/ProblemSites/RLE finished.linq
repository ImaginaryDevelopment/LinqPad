<Query Kind="FSharpProgram" />


let debug = true
type RleState = {Current: byte option; Count: int; OutputRev:byte list} with 
    member x.Add v =
        let result = 
            match x.Current with
            | None -> {Current = Some v; Count = 1; OutputRev=x.OutputRev}
            | Some c when v = c -> { x with Count = x.Count + 1}
            | Some c -> { Current = Some v; Count = 1; OutputRev = c :: byte x.Count :: x.OutputRev }
        //(x,result).Dump("add")
        result
    member x.Finish () = 
        match x.Current with 
        | Some c -> 
            //x.Dump("finishing")
            c :: byte x.Count :: x.OutputRev |> List.rev
        | None -> x.OutputRev |> List.rev
        
// placeholder
let runLengthEncode (input:byte[]) = 
    let folder (state:RleState) (b:byte) = 
        state.Add b
    input
    |> Seq.fold folder {Current=None; Count = 0; OutputRev = List.empty}
    |> fun x -> 
        let output = x.Finish()
        (input,output).Dump()
        output

let uncurry f (a, b) = f a b

let compareBytes = Seq.compareWith (fun a b ->
    if a > b then 1
    elif a < b then -1
    else 0)	
    
let test a b : bool =
    try
        let actual=a |> Array.ofList |> runLengthEncode
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