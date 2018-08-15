<Query Kind="FSharpProgram" />

// indentation aware clustering kinda
// find the longest type in here

let test = """

type X() =
    1
    2
type Y() =
    1
    2
    3
type Z() =
    1
    2
module T =
    type B() =
        1
    type C() =
        1
        2
        3
        4
"""

let (|RMatch|_|) p x = 
    let m = Regex.Match(x,p)
    if m.Success then
        Some m
    else None
   
let (|ValueString|_|) =
    function
    | null | "" -> None
    | x when String.IsNullOrWhiteSpace x -> None
    | x -> Some <| ValueString x
    
let (|Whitespace|_|) = 
    function
    | null | "" -> None
    | x when String.IsNullOrWhiteSpace x -> Some Whitespace
    | _ -> None
    
let (|TypeDeclaration|Line|) =
    function
    | null | "" | Whitespace -> Line 0
    | RMatch "^(\s*)(type (.+)\(.*\)\s*=)?" m ->
        match m.Groups.[2].Value with
        | "" 
        | null -> Line m.Groups.[1].Length
        | _ -> 
            TypeDeclaration (m.Groups.[1].Length,m.Groups.[3].Value)
    // match on anything that's not an empty line or a comment line
    | RMatch "^(\s*)[^/]+" m ->
        Line m.Groups.[1].Length
    | x -> failwithf "bad pattern? %s" x

type TypeInfo = {TypeStartIndentation:int;Lines:int;TypeDeclarationText:string}
type LineWalkState =
    | NotInAType
    | InAType of TypeInfo
    
let getOptMax x =
    function
    | Some y -> max x y
    | None -> x
    >> Some
    
let lines =  
    test.SplitLines()
    |> Seq.choose (|ValueString|_|)
    |> List.ofSeq
let startType (li,typeText) =
    printfn "Starting type %s" typeText
    InAType {TypeStartIndentation=li; Lines=1;TypeDeclarationText=typeText}
    
lines
|> Seq.fold(fun (currentState:LineWalkState,longestOpt: int option) (line:string) -> 
    // if we aren't in a type then just carry longest type length option forward
    // if we are starting a type then 
    match currentState,line with
    | NotInAType, Line li ->
        printfn "skipping line %s" line
        NotInAType, longestOpt
    | NotInAType, TypeDeclaration (ti,x) ->
        startType (ti,x), getOptMax 1 longestOpt
    | InAType ti, Line li ->
        if li <= ti.TypeStartIndentation then // type definition is over
            printfn "Ending type(%s) with line %s" ti.TypeDeclarationText line
            NotInAType, getOptMax ti.Lines longestOpt
        else
            printfn "Continuing type %s" line
            let x = ti.Lines + 1
            InAType {ti with Lines = x}, getOptMax x longestOpt
    
    | InAType ti, TypeDeclaration (li,x) ->
        // what to do when the indentation = type's indentation or is less, but there's no text? that doesn't end a type
        if li <= ti.TypeStartIndentation then // type definition is over
            printfn "Ending type(%s) with line %s" ti.TypeDeclarationText line
            startType (li,x), getOptMax ti.Lines longestOpt
        else
            printfn "Continuing type %s" line
            let x = ti.Lines + 1
            InAType {ti with Lines = x}, getOptMax x longestOpt
//    | x,y -> (x,y).Dump("fail"); invalidOp (sprintf "%A" x)
    

) (NotInAType, None)
|> function
    |InAType ti, longestOpt ->
        getOptMax ti.Lines longestOpt
    | NotInAType, longestOpt -> 
        longestOpt 
|> Dump
|> ignore