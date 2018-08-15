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
    type D() =
        1
type E() = 
    1
    2
    3
    4
    5
    
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

type TypeInfo = {TypeStartIndentation:int;Lines:int;TypeDeclarationText:string; TypeStartLine:int}
type LineWalkState =
    | NotInAType
    | InAType of TypeInfo
    
    
let maxIndex,lines =  
    let lines =  test.SplitLines()
    lines.Length - 1,lines
    |> Seq.mapi(fun i x ->
        i,x
    )
    |> Seq.choose (fun (i,x) -> match x with |ValueString x -> Some (i,x) | _ -> None)
    |> List.ofSeq
type LongestInfo = {LineIndex:int;Length:int;EndLineIndex:int}    
let getOptMax (li,typeLength, eLi) =
    let def = {LineIndex=li; Length=typeLength;EndLineIndex=eLi}
    function
    | Some y -> if y.Length > typeLength then y else def
    | None -> def
    >> Some
let startType debug (lineIndex,lineIndent,typeText,typeEndIndex) =
    if debug then
        printfn "Starting type %s" typeText
    InAType {TypeStartIndentation=lineIndent; Lines=1;TypeDeclarationText=typeText;TypeStartLine=lineIndex}
    
let foldLines debug (currentState:LineWalkState,longestOpt: LongestInfo option) (lineIndex,line) = 
    // if we aren't in a type then just carry longest type length option forward
    // if we are starting a type then 
    match currentState,line with
    | NotInAType, Line li ->
        if debug then 
            printfn "skipping line %s" line
        NotInAType, longestOpt
    | NotInAType, TypeDeclaration (ti,x) ->
        startType debug (lineIndex,ti,x,lineIndex), getOptMax (lineIndex,1, lineIndex) longestOpt
    | InAType ti, Line li ->
        if li <= ti.TypeStartIndentation then // type definition is over
            printfn "Ending type(%s) with line %s" ti.TypeDeclarationText line
            NotInAType, getOptMax (ti.TypeStartLine,ti.Lines,li) longestOpt
        else
            printfn "Continuing type %s" line
            let x = ti.Lines + 1
            InAType {ti with Lines = x}, getOptMax (ti.TypeStartLine,x,li) longestOpt
    
    | InAType ti, TypeDeclaration (li,x) ->
        // what to do when the indentation = type's indentation or is less, but there's no text? that doesn't end a type
        if li <= ti.TypeStartIndentation then // type definition is over
            if debug then
                printfn "Ending type(%s) with line %s" ti.TypeDeclarationText line
            startType debug (lineIndex,li,x,lineIndex), getOptMax (ti.TypeStartLine,ti.Lines,lineIndex) longestOpt
        else
            if debug then
                printfn "Continuing type %s" line
            let x = ti.Lines + 1
            InAType {ti with Lines = x}, getOptMax (ti.TypeStartLine,x, lineIndex) longestOpt
//    | x,y -> (x,y).Dump("fail"); invalidOp (sprintf "%A" x)
    

let getLongestType debug = Seq.fold (foldLines debug) (NotInAType, None)
    
lines
|> getLongestType true
|> function
    |InAType ti, longestOpt ->
        getOptMax (ti.TypeStartLine,ti.Lines,maxIndex) longestOpt
    | NotInAType, longestOpt -> 
        longestOpt 
|> Dump
|> ignore