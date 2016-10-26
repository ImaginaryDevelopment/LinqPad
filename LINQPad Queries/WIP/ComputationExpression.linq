<Query Kind="FSharpProgram" />

// trying to learn computation expressions
//http://theburningmonk.com/2012/01/f-retry-workflow/

#if LINQPAD // doesn't actually work right in linqpad, but conveys intention, hopefuly
let dumpt (title:string) x = x.Dump(title); x
#else
let dumpt (title:string) x = printfn "%s:%A" title x; x    
#endif

module AttemptBuilding =
    // attempted, failed to implement, may not understand computation expressions well enough yet
//    type AttemptResult<'a> = {Result: 'a option; AttemptsMade: int}
//    type Attempt<'a> = (unit -> 'a AttemptResult)
    type AttemptResult<'a> = 
        | Failure of i:int
        | Success of i:int*result: 'a
    type private InternalAttempt<'a> = (int -> 'a AttemptResult)
    type Attempt<'a> = (unit -> 'a option)
    let succeed x = fun () -> Some x
    let fail = fun () -> None
    let runAttempt (a:Attempt<'a>) = a()
    type Attempt = 
        static member Fail() = fail
        static member Succeed x = succeed x
        static member Run (a: Attempt<'a>) = 
            match runAttempt a with
            | Some x -> Success (1,x)
            | None -> Failure 1
        
    let rec bind p rest i max = 
        try 
            match runAttempt p with
            |Some r -> (rest r)
            |None -> fail
        with 
        | _ when i < max -> bind p rest (i+1) max
        | _ -> fail
        
    let delay f = fun () -> runAttempt (f())
    type AttemptBuilder (maxRetry) =
        member __.Return x = succeed x
        member __.ReturnFrom (x:Attempt<'a>) = x
        member __.Bind (p,rest) = bind p rest 1 maxRetry
        member __.Delay f = delay f
        member __.Zero () = fail
    let attempt = AttemptBuilder(1)
    let retry max = AttemptBuilder(max)
    
    
open AttemptBuilding


module AttemptTests = 
    let expectedAttempt: Attempt<int> = attempt { failwith "oops"}
    let ``no retry`` () = runAttempt <| attempt {
        let! a = expectedAttempt
        return a
    }
    
    let ``retry three times makes three attempts`` () = 
        let mutable tries = 0
        let f () = 
            tries <- tries + 1
            expectedAttempt()
            
        let _attemptResult = 
            Attempt.Run <| retry 3 {
                let! a = f
                return a
            }
        tries = 3 |> Debug.Assert 
        if tries <> 3 then failwithf "Tries should have been 3 but were %i" tries
    let successTest () = Attempt.Run <| attempt { return 42 }
    let failIfBig n = attempt {
        printfn "fail if n > 1000"
        if n > 1000 then return! Attempt.Fail() else return n
    }
    let failureTest() = Attempt.Run <| retry 3 {
        let! n = failIfBig(1001)
        return n
    }
open AttemptTests

type TestingLogStrategy =
    | LogBuilder
    | WrapI
    
module LogBuilding = //https://fsharpforfunandprofit.com/posts/computation-expressions-intro/
    type LoggingBuilder() = 
        let log p = printfn "expression is %A" p
        member __.Bind(x,f) =
            log x
            f x
        member __.Return x = 
            x
    let logger = LoggingBuilder()

// print something before each test, since the way they were setup before, some of the tests were firing before called (no parens at the end of test function)
//let runTest i f = 
//    printfn "Starting test%i" i
//    f()
//    |> dumpt (sprintf "test%i result" i)
//    |> ignore
let runTests ts = 
    match ts with 
    |WrapI ->
        // print something before each test, since the way they were setup before, some of the tests were firing before called (no parens at the end of test function)
        let runTest i f = 
            printfn "Starting test%i" i
            f()
            |> dumpt (sprintf "test%i result" i)
            |> ignore
            
        runTest 1 ``no retry``
        runTest 2 ``retry three times makes three attempts``
        runTest 3 successTest
        runTest 4 failureTest
        
    | LogBuilder ->
        LogBuilding.logger {
            let! t1Result = ``no retry``()
            let! t2Result = ``retry three times makes three attempts``()
            let! t3Result = successTest()
            let! t4Result = failureTest()
            return t1Result,t2Result,t3Result,t4Result
        }
        |> dumpt "test results"
        |> ignore
        
runTests LogBuilder //WrapI