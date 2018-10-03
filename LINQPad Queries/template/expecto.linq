<Query Kind="FSharpExpression">
  <NuGetReference>Expecto</NuGetReference>
  <NuGetReference>FSharp.Core</NuGetReference>
  <Namespace>Expecto</Namespace>
</Query>


let trim1 (d:char) (x:string) = x.Trim d

let (|RMatch|_|) p x =
    let m = Regex.Match(x,p)
    if m.Success then Some m
    else None
    
let (|AnyRMatch|_|) ps x =
    ps
    |> Seq.choose(fun p -> match x with |RMatch p m -> Some m | _ -> None)
    |> Seq.tryHead
let (|Arg|_|) flag =
    let patterns = ["/(\w+)";"-(\w+)";"--(\w+)"] |> List.map(sprintf "%s(?::(.*))?")
    function
    |null |"" -> None
    | AnyRMatch patterns m when m.Groups.[1].Value = flag -> Some (m.Groups.[2].Value)
    | _ -> None

let tests = 
    testList "FileMatching" [
        test @"/file:""C:\Hello world""" {
                let expected = @"C:\Hello world"
                let arg = sprintf "/file:\"%s\""expected
                match arg with
                | Arg "file" quoted ->
                    let actual = trim1 '"' quoted
                    Expect.equal actual expected "did not match"
                | _ -> failwithf "did not match"
            }
        
    ]
let config = defaultConfig.appendSummaryHandler(fun trs ->
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