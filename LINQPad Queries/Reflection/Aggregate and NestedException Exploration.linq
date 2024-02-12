<Query Kind="FSharpProgram" />

type Strategy =
    | UseInner // ignore aggregate exceptions entirely
    | UseFlatten
    | ManualRec

let rec walk fEx (depth,locator) (ex:exn) =
    [
        yield depth, locator, ex
        let depth = depth + 1
        for i, ex in fEx ex |> List.choose Option.ofObj |> List.indexed do
            yield! walk fEx (depth, sprintf "%s.%i.%i" locator depth i) ex
    ]
let walkInner = walk (fun ex -> [ex.InnerException])

let walkFlatten =
    let f (ex:exn) = 
        match ex with
        | :? AggregateException as aEx ->
            [aEx.Flatten().InnerException]
        | _ -> [ex.InnerException]
    walk f
    
let walkManualRec =
    let f (ex:exn) =
        match ex with
        | :? AggregateException as aEx ->
            aEx.InnerExceptions |> List.ofSeq
        | _ -> [ ex.InnerException ]
    walk f
    
let walkEx strategy (ex:exn) =
    // locator because siblings that have children would wind up with the same order int
    let rec unfoldOne (depth, locator, ex) =
        match strategy with
        | UseInner -> walkInner (depth,locator) ex
        | UseFlatten ->
            walkFlatten (depth,locator) ex
        | ManualRec ->
            walkManualRec (depth,locator) ex
    unfoldOne (0,"root", ex)

[
    Exception()
    Exception("Ah",innerException=Exception())
    upcast AggregateException([Exception("A", Exception("A.inner", Exception("A.inner.inner"))); Exception("B", AggregateException("B.inner", Exception("B.inner2")))])
]
//|> List.map (walkEx Strategy.UseFlatten)
|> List.map (walkManualRec (0, "root"))
|> Dump
|> ignore