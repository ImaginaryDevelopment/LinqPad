<Query Kind="FSharpExpression">
  <NuGetReference>Expecto</NuGetReference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <Namespace>Expecto</Namespace>
</Query>

let trim1 (d:char) (x:string) = x.Trim d

// whatever the idler generates, this is the return formula, should be linear
let getReturnRate (baseRate:float<_>) (level:int64) =
    let result = float level * float (decimal baseRate)
//    printfn "%f,%f -> %f" baseRate (float level) result
    result
let getPrice multiplier baseCost baseRate (level:int64) =
    let effect = getReturnRate baseRate level
    baseCost * multiplier ** ((effect/baseRate) - 1.0)

let tests = 
    testList "test" [
    
        testList "Basic" [
            test "Hello world" {
                let expected = "hello"
                let actual = "hello"
                Expect.equal actual expected "what a world"
            }
        ]
        testList "Income" [
            testCase "Cursor" <|
                fun () ->
                    let rates =
                        [   0.1
                            0.2
                            0.3
                            0.4
                            0.5
                            0.6
                            0.7
                            0.8
                        ]
                        |> List.map float
                    rates
                    |> List.iteri(fun i expected ->
                        let i = i+1
                        let actual = getReturnRate rates.[0] (int64 i)
                        Expect.floatClose Accuracy.veryHigh actual expected <| sprintf "Cursor-%i" i 
                    )
                        
                    
        ]
        testList "Idle" [
        // fsCheck
//            testProperty "Level 1 should identity the baseCost" <|
//                fun multiplier baseRate price ->
//                    let expected = price
//                    let actual = getPrice multiplier price baseRate 1L
//                    Expect.equal actual expected "not an identity"
//                    ()
            testCase "1" <|
                fun () ->
                    let expected = 15.0
                    let actual = getPrice 1.15 expected 0.1 1L
                    Expect.equal actual expected "2 case fail"
            testCase "2" <|
                fun () ->
                    let expected = 17.25
                    let actual = getPrice 1.15 15.0 0.1 2L
                    Expect.equal actual expected "2 case fail"
            testCase "10" <|
                fun () ->
                    let expected = 52.76814438
                    let actual = getPrice 1.15 15.0 0.1 10L
                    Expect.floatClose Accuracy.veryHigh actual expected "10 case"
            
        ]
    ]
let config = {defaultConfig with verbosity= Expecto.Logging.LogLevel.Debug}.appendSummaryHandler(fun trs ->
    if trs.successful then
        printfn "%i completed" trs.passed.Length
    else
        trs.failed
        |> List.append trs.errored
        |> List.choose(
            function
            |{name=n}, {result=Expecto.Impl.Failed msg } -> Some (n,msg)
            |{name=n}, {result=Expecto.Impl.Error ex} -> Some(n, sprintf "%A" ex)
            | _ -> None
            
        )
        |> fun x -> x.Dump()
)
tests
|> runTests config