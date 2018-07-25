<Query Kind="FSharpExpression" />

// convert between standard and scientific notation
let sci = float >> fun x -> x.ToString("0.##e-0")
let (|Regex|_|) p x =
    let m = Regex.Match(x,p)
    if m.Success then
        Some m
    else None
let (|Int|_|) x =
    match Int32.TryParse x with
    | true, x -> Some x
    | false, _ -> None
let (|Dec|_|) x =
    match System.Decimal.TryParse x with
    | true, x -> Some x
    | false, _ -> None
let testCases =
    [   "1B"
        "1T"
        "6Sx"
        "3.25Qi"
        "749.98M"
    ]
let (|EqualsI|_|) y x =
    if not <| isNull y && not <| isNull x && String.Equals(x,y,StringComparison.InvariantCultureIgnoreCase) then
        Some ()
    else None
    
let translateStandard = // https://officespace.zendesk.com/hc/en-us/articles/115000593531-Scientific-Notation-Large-Numbers-Guide
    function
    | EqualsI "K" -> Some 1.0e3
    | EqualsI "M" -> Some 1.0e6
    | EqualsI "B" -> Some 1.0e9
    | EqualsI "T" -> Some 1.0e12
    | EqualsI "Qa" -> Some 1.0e15
    | EqualsI "Qi" -> Some 1.0e18
    | EqualsI "Sx" -> Some 1.0e21
    | EqualsI "Sp" -> Some 1.0e24
    | EqualsI "Oc" -> Some 1.0e27
    | EqualsI "No" -> Some 1.0e30
    | x ->
        printfn "failed to match %s" x
        None
    
let processNumber x : float option =
    let rec f canRecurse x : float option=
        printfn "Trying to process %A" x
        match x with
    //Util.ReadLine("Number?") with
        | Int x -> 
            printfn "Processed as %i" x
            float x
            |> Some
        | Dec x ->
            float x
            |> Some
        // standard notation
        | Regex @"\$?(\d+)(\.\d+)?(\w{0,3})" m -> 
            printfn "Regexed %A" x
            let wholePart = m.Groups.[1].Value
            let decPart = m.Groups.[2].Value
            let namePart = m.Groups.[3].Value
            printfn "RegexValues:(%s,%s,%s)" wholePart decPart namePart
            if canRecurse then
                match wholePart + decPart |> f false, translateStandard namePart with
                | Some v,Some mult -> 
                    mult * v |> Some
                | _ -> None
            else None
        | x -> 
            printfn "could not process %A" x
            None
    f true x
testCases
|> Seq.map (fun x -> x, processNumber x |>function |Some x -> sci x |None -> "")
|> fun x -> x.Dump("tests")
Util.ReadLine "Number?"
|> processNumber