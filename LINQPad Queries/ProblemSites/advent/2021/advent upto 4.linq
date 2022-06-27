<Query Kind="FSharpProgram" />

// need the sample input data
let getClipLines name =
    Util.Cache( (fun () -> System.Windows.Forms.Clipboard.GetText()) , name).Split("\r\n")
    |> Array.map (fun x -> x.Trim())
    |> fun x ->
        printfn "Taking in %i lines" x.Length
        x
let getInts() =
    getClipLines "ints"
    |> Seq.map(fun x -> x.Trim() |> int)
    
module Seq =
    let any x =
        x
        |> Seq.exists(fun _ -> true)
    // keep pulling until you find one, and include it
    let takeWhileInc f x =
        let mutable found = false
        x
        |> Seq.filter(fun x ->
            if found then false
            elif not <| f x then
                found <- true
                true
            else true
        )
module Advent1 =
    let run1 values =
        (0, values |> Seq.pairwise)
        ||> Seq.fold(fun i (p,n) ->
            if n > p then i + 1 else i
        )
    let run2 (values:int seq) =
        (0,values |> Seq.windowed 3 |> Seq.map Seq.sum |> Seq.pairwise)
        ||> Seq.fold(fun i (p,n) ->
            if n > p then i + 1 else i
        )
        
let (|Int|) x = Int32.Parse x

module Advent2 =
    let getEndCoords (values: string seq) =
        ((0,0), values)
        ||> Seq.fold(fun (x,y) line ->
            line.Split(" ") |> List.ofSeq
            |> function
                | "forward"::v::[] -> (x + int v, y)
                | "down"::v::[] -> (x, y + int v)
                | "up"::v::[] -> (x, y - int v)
                | _ -> failwithf "unexpected value: '%s'" line
        )
    let run1 values =
        getEndCoords values
        |> fun (x,y) -> x * y
    let runOrders (values: string seq) =
        ((0,0,0), values)
        ||> Seq.fold(fun (x,y,aim) line ->
            line.Split(" ") |> List.ofSeq
            |> function
                | "forward"::Int v::[] -> (x + v, y + v*aim , aim )
                | "down"::Int v::[] -> (x, y, aim + v)
                | "up"::Int v::[] -> (x, y, aim - v)
                | _ -> failwithf "unexpected value: '%s'" line
        )
    let run2 values =
        runOrders values
        |> fun (x,y,_) -> x * y
    let runFromClipboard() =
        getClipLines "advent3"
        |> run2
let binaryCharToInt =
    function
    | '1' -> 1
    | '0' -> 0
    | c -> failwithf "Unexpected c: %c" c
    
let intOfBSeq x = x |> Array.map string |> String.concat "" |> sprintf "0b%s" |> int
 
let binaryStringToIntArray (x:string) =
    x.ToCharArray()
    |> Array.map binaryCharToInt
    
type BinaryGrouping<'t> = {
    Zero: 't
    One: 't
}
type BitCommonality = 
    | MostCommon
    | LeastCommon
    
module Advent3 =
    let getGammaAndEpsilon (values:string seq) =
        values
        |> Seq.map binaryStringToIntArray
        //|> Seq.map(fun x -> x.ToCharArray() |> Array.map (function | '1' -> 1 | '0' -> 0 | c -> failwithf "Unexpected c: %c" c))
        |> Array.transpose
        |> Array.map(fun pos -> pos |> Seq.groupBy id |> Seq.map(fun (g,x) -> g, x |> Seq.length))
        |> Array.map(Seq.sortByDescending snd >> Seq.map fst>> Array.ofSeq)
        |> Array.transpose
        |> Array.map (Seq.map string >> String.concat "")
        |> List.ofSeq
        |> function
            | gamma::ep::[] -> sprintf "0b%s"gamma |> int, sprintf "0b%s" ep |> int
            | x -> failwithf "unexpected length: %i in %A" x.Length x
    let run1 values =
        values
        |> getGammaAndEpsilon
        |> fun (x,y) -> x * y
    let transposeBinaryGroups =
        function
        | (k1,v1) :: (_,v2) ::[] ->
            if k1 = 0 then {Zero=v1;One=v2}
            else {Zero=v2;One=v1}
        | x ->
            failwithf "Unexpected shape %A" x
        
    let getBinaryCountByPos (values:int[] seq) =
        values
        |> Array.transpose
        |> Array.map(Seq.groupBy id >> Seq.map(fun (g,v) -> g, Seq.length v) >> List.ofSeq)
        |> Array.map(fun x ->
            let x =
                match x with
                | (0,v) :: [] -> (0,v)::(1,0)::[]
                | (1,v) :: [] -> (1,v)::(0,0)::[]
                | x -> x
            try
                transposeBinaryGroups x
            with _ ->
                (values,x).Dump("eh?")
                reraise()
        )
        
    let getKeepBit bc pos (values:int[] seq) =
        let bitCount =
            let counts = values |> Seq.map (Array.skip pos) |> getBinaryCountByPos
            if counts.Length < 1 then
                failwithf "Requested pos %i with len %i" pos counts.Length
            //(counts.Dump("at " + string pos))
            counts.[0]
        match bitCount.Zero = bitCount.One, bitCount.Zero < bitCount.One, bc with
        | true, _, LeastCommon -> 0 // if equally common, lc keeps 0
        | true, _, MostCommon -> 1 // if equally common, mc keeps 1
        | _, true, LeastCommon -> 0 // less zeros, lc keeps 0
        | _, false, LeastCommon -> 1 // more zeros, lc keeps 1
        | _, true, MostCommon -> 1 // less zeros, mc keeps 1
        | _, false, MostCommon -> 0 // more zeros, mc keeps 0
        
    let filterByBit bc pos (values:int[] seq) =
        let keepBit = getKeepBit bc pos values
        values
        |> Seq.filter(fun x -> x.[pos] = keepBit)
        |> List.ofSeq
            
    let runBitCriteria bc values =    
        (values,0)
        |> Seq.unfold(fun (keeps,i) ->
            match keeps with
            | _::[] -> None // if there's only one number left, use it
            | _ ->
                //printfn "Filtering [%i]" i
                let keeps = filterByBit bc i keeps |> List.ofSeq
                //(keeps,i).Dump("yay?")
                Some(keeps, (keeps,1 + i))
        )
        |> Seq.collect id
        |> Seq.tryLast
        |> Option.map intOfBSeq
        
    let runOxy = runBitCriteria MostCommon
    let runCO2 = runBitCriteria LeastCommon
    
    let runBoth values =
        values
        |> Array.map binaryStringToIntArray
        |> List.ofSeq
        |> fun x -> x |> runOxy , x |> runCO2
    let runFromClipboard() =    
        getClipLines "advent4"
        |> runBoth
        |> function
            | Some a, Some b -> a * b
            | x,y -> failwithf "A value couldn't be calculated(%A,%A)" x y
        
let trim (x:string) = x.Trim()    
let split (delimiter:string) (x:string) = x.Split(delimiter)

module Advent5 = 
    type Board = {
        Values:int[][]
        Matched:int list
    }
    type BoardType =
        | InPlay of Board
        | Winner of round:int * Board
        
    let isWinner =
        function
        | Winner _ -> true
        | _ -> false
        
    let toBoard (values:string list) =
        try
            {Values =
                values
                |> List.map (trim >> split " " >> Seq.filter (String.IsNullOrWhiteSpace>>not) >> Seq.map int >> Array.ofSeq)
                |> Array.ofSeq
             Matched = List.empty}
        with _ ->
            values.Dump("eh?")
            reraise()
            
    let processNumber called ({Values=v} as board) =
        v
        |> Array.indexed
        |> Array.tryPick(fun (i, line) -> line |> Array.tryFindIndex(fun v -> v = called) |> Option.map(fun x -> i,x))
        |> function
            | Some(y,x) ->
                v
                |> Array.mapi(fun yi row -> if yi <> y then row else (row |> Array.mapi(fun xi v -> if xi <> x then v else -1)))
                |> fun v -> {board with Values = v; Matched = called:: board.Matched}
            | None -> board
            
    let checkWin round ({Values=v} as board) =
        // check for winning rows
        if  v |> Array.exists(Array.forall((=) -1)) then
            Winner (round,board)
        elif v |> Array.transpose |> Array.exists(Array.forall((=) -1)) then Winner (round,board)
        else InPlay board
        
    let runNumbers stopOnFirst (lines:string seq) =
        lines
        |> List.ofSeq
        |> function
            | numbers::_::boards ->
                numbers.Split(",") |> Array.map int , boards |> List.chunkBySize 6 |> List.map(List.truncate 5) |> List.map toBoard
            | x -> (x.Dump("bad")); failwith "bad input"
        |> fun (n,boards) ->
            let result =
                let mutable weHaveAWinner = false
                (boards|> List.map InPlay, n |> Seq.indexed)
                ||> Seq.scan(fun boards (round,n) ->
                    if weHaveAWinner && stopOnFirst then
                        printfn "We had a winner"
                        List.empty
                    else
                        boards
                        |> List.map(
                            function
                            | InPlay x ->
                                match processNumber n x |> checkWin round  with
                                | Winner _ as x ->
                                    weHaveAWinner <- true
                                    //printfn "Setting winner"
                                    x
                                | x -> x
                            | x -> x)
                )
            result
    let getScore b =
        let vSum = b.Values |> Seq.concat |> Seq.filter(fun i -> i >= 0) |> Seq.sum
        let lastCall = b.Matched.[0]
        lastCall * vSum
        
    let getFirstWinner lines =
        let stopOnFirst = true
        runNumbers stopOnFirst lines
        |> Seq.indexed
        |> Seq.takeWhile(snd >> Seq.any) // we stop getting values once we have a winner if stopOnFirst is true
        |> Seq.tryLast // get the first winner (should be the last item in the seq
        |> Option.map (fun (_round,b) ->
            b
            |> List.indexed
            |> List.choose(
                function
                | bn,Winner (round,b) -> 
                    let vSum = b.Values |> Seq.concat |> Seq.filter(fun i -> i >= 0) |> Seq.sum
                    let lastCall = b.Matched.[0]
                    //let score = lastCall * vSum
                    let score = getScore b
                    
                    let text = sprintf "%i * %i = %i" vSum lastCall score
                    Some (round,bn,score, text, b)
                | _ -> None
            )
        ) 
        
    let getLastWinner lines =
        runNumbers false lines
        |> Seq.indexed
        |> Seq.tryFind(snd>>Seq.forall isWinner)
        |> Option.map(snd)
        |> Option.map(fun boards -> boards |> List.map(function | Winner(r,b) -> r,b | _ -> failwith "Found non winner!"))
        |> Option.map(fun boards ->
            boards
            |> List.sortByDescending fst
            |> List.map(fun (r,b) -> r,b |> getScore)
        )
        //|> Option.map(fun (_,x) -> 
        //    let winInfo =
        //        x
        //        |> List.map(
        //            function
        //            | Winner(r,b) -> r, b
        //            | _ -> failwith "Found non winner!"
        //        )
        //    winInfo
        //    |> List.maxBy(fun (_,b) ->
        //        //if r <> r2 then failwithf "round counters differ %i vs %i (%i)" r r2 b.Matched.Length
        //        //if r <> b.Matched.Length - 1 then failwithf "Round counter and matched differ: %i vs %i" r <| b.Matched.Length - 1
        //        //(r,b).Dump("eh?")
        //        b.Matched.Length
        //    )
        //    |> fun (r,b) -> r - 1, b, b |> getScore
        //)
        

let sample = "advent5"    
let real = "advent5x"
getClipLines real
//|> Advent5.getFirstWinner
|> Advent5.getLastWinner
|> Dump
|> ignore



