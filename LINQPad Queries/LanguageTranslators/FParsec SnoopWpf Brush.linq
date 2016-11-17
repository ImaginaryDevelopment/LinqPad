<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll</Reference>
  <NuGetReference>FParsec</NuGetReference>
  <Namespace>FParsec</Namespace>
</Query>

//parse brush pasting from snoop

#if INTERACTIVE
#I @"..\LINQPad\NuGet.FW46\FParsec\FParsec.1.0.2\lib\net40-client\" // references AppData\local\ ...  since . is %localappdata%\TEMP
// #I @"C:\Users\Brandon\AppData\Local\LINQPad\NuGet.FW46\FParsec\FParsec.1.0.2\lib\net40-client\"
#r "FParsecCS.dll"
#r @"FParsec.dll"
#endif

open FParsec

module Seq =
    let any items = items |> Seq.exists (fun _ -> true)

let samples = [
    """<LinearGradientBrush StartPoint="0.5,0" EndPoint="0.5,1" xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation">
    <LinearGradientBrush.GradientStops>
    <GradientStop Color="#FFFFFFFF" Offset="0" />
    <GradientStop Color="#FFF5F5F5" Offset="1" />
    </LinearGradientBrush.GradientStops>
</LinearGradientBrush>"""

    """<LinearGradientBrush StartPoint="0.5,0" EndPoint="0.5,1" xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"><LinearGradientBrush.GradientStops><GradientStop Color="#FFFFFFFF" />
<GradientStop Color="#FFD4D4D4" Offset="1" /></LinearGradientBrush.GradientStops></LinearGradientBrush>"""
    """<LinearGradientBrush StartPoint="0.5,0" EndPoint="0.5,1" xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"><LinearGradientBrush.GradientStops><GradientStop Color="#FF5B5B5B" Offset="1" />
<GradientStop Color="#FF868686" /><GradientStop Color="#FF4F4F4F" Offset="0.42" /><GradientStop Color="#FF0E0E0E" Offset="0.43" /></LinearGradientBrush.GradientStops></LinearGradientBrush>"""
]

let test p str = 
    match run p str with
    |Success(result, _, _)   -> printfn "Success: %A" result
    |Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
type GStop = {Color:string;Offset:float option}
type Lgb = {StartPoint:option<float*float>;EndPoint:option<float*float>;Stops : GStop list}

let parseBrush = 
    // pipe2 (pstring "<LinearGradientBrush StartPoint=\"" >>. pfloat) ((pstring ",") >>. pfloat)  (fun x y -> x,y)
    let parsePoint = pstring "\"" >>. pfloat .>>. (pstring "," >>. pfloat) .>> pstring "\""
    let point= (pstringCI "startpoint" <|> pstringCI "endpoint") >>. (skipString "=" >>. parsePoint)
    let attrNamed s pValue = pstring s >>. (pstring "=" >>. pstring "\"" >>. pValue .>> pstring "\"")
    let localName =     
        let targetStarter = fun c -> isAsciiLetter c
        let targetContinue = fun c -> isAsciiLetter c || c = '.'
        identifier (IdentifierOptions(isAsciiIdStart = targetStarter, isAsciiIdContinue = targetContinue))
    let strLiteral = 
        let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
        let escapedChar = 
            pstring "\\" 
            >>. (anyOf "\\nrt\"" |>> function
                                        | 'n' -> "\n"
                                        | 'r' -> "\r"
                                        | 't' -> "\t"
                                        | c   -> string c)

        between (pstring "\"") (pstring"\"") (stringsSepBy normalCharSnippet escapedChar)

    let attr = localName .>>. (pstring "=" >>. strLiteral .>> spaces)
    let lgbTag = 
        // assuming for now that startpoint and endpoint are always in the same order
        pstring "<LinearGradientBrush" >>. opt (spaces >>. point) .>>. opt (spaces >>. point) .>> spaces .>> skipMany attr .>> (pstring ">") .>> optional (newline .>> spaces)
        |>> fun (startp,endp) -> {StartPoint = startp; EndPoint=endp;Stops = list.Empty}
    let selfcloser p = p .>> spaces .>> (pstring "/>")
    let color = 
        let colorStarter = fun c -> isAsciiLetter c || isDigit c
        let colorCont = colorStarter
        optional (pstring "#") >>. (identifier (IdentifierOptions(isAsciiIdStart=colorStarter, isAsciiIdContinue= colorCont)))
    // attrNamed "Color" color .>>. (spaces >>. attrNamed "Offset" pfloat
    let gradientStop = 
        pstringCI "<GradientStop" >>. spaces >>. many attr .>> spaces .>> (skipString "/>") 
        |>> (fun attrs ->
            //attrs.Dump()
            // WIP: for now, need to figure out attrs first
            let attrs = attrs |> Map.ofSeq
            let offset = 
                let key = "Offset"
                if attrs.ContainsKey key then
                    attrs.[key] |> float |> Some
                else None
            {Color=attrs.["Color"]; Offset=  offset}
            )
    let lgbWithStops =
        lgbTag .>>. (pstringCI "<LinearGradientBrush.GradientStops>" >>. many gradientStop)
    lgbWithStops

// test (pstring "<") sampleText
samples
|> Seq.iter (test parseBrush)

//test parseBrush sampleText
"finished test run,starting addl code".Dump()
let sampleText = samples.[samples.Length - 1]
let tryParseBrush text = 
    match run parseBrush text with
    | Success (result, _, _) ->
        match result with
        |(lgb, stops) -> {lgb with Stops= stops} |> Some
    |Failure(errorMsg, _, _) ->printfn "Failure: %s" errorMsg; None

match run parseBrush sampleText with
|Success (result, _, _) ->
    match result with
    | (lgb,stops) -> {lgb with Stops = stops} |> printfn "yay deconstructed! %A"
|Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg

module Transformers =
    type Lgb with
        member x.ToFSharp () = 
            let sb = System.Text.StringBuilder()
            seq{
                yield "let gb = LinearGradientBrush()"
                match x.StartPoint with
                |Some (x,y) -> yield sprintf "gb.StartPoint <- Point(%f,%f)" x y
                |None -> ()
                match x.EndPoint with
                |Some (x,y) -> yield sprintf "gb.EndPoint <- Point(%f,%f)" x y
                |None -> ()
                let getOffset (x: GStop) = defaultArg (x.Offset |> Option.map (sprintf ",%f")) String.Empty
                if Seq.any x.Stops then
                    yield "["
                    yield! x.Stops |> Seq.map(fun gStop -> sprintf "    GradientStop(argbFromHex \"#%s\",%f)" gStop.Color gStop.Offset) // doesn't account for proper F# formatting of a list
                    yield "]"
                    yield sprintf "|> Seq.iter gb.GradientStops.Add"
                }
            |> Seq.iter (sb.AppendLine >> ignore)
            sb.ToString ()
        member x.ToCss() = // https://developer.mozilla.org/en-US/docs/Web/CSS/linear-gradient
            //x.Dump("toCssing")
            let sb = System.Text.StringBuilder()
            // <LinearGradientBrush StartPoint="0.5,0" EndPoint="0.5,1"> means Starts at 50% x 0% y and ends at 50% x, 100% y
            seq{
                yield "{"
                let toTopOrToBottom = if x.Stops.Length < 2 || (x.Stops.[0].Offset.IsSome && x.Stops.[1].Offset.IsSome && x.Stops.[0].Offset.Value < x.Stops.[0].Offset.Value) then "top" else "bottom"
                x.Stops.Dump() |> ignore
                let stops = x.Stops |> Seq.map(fun stop -> sprintf "#%s %s%%" (stop.Color.Substring(3)) (if stop.Offset.IsSome then stop.Offset.Value * 100. |> Convert.ToInt16 |> string else String.Empty) ) |> delimit ", "
                yield sprintf "    background-image: linear-gradient(to %s, %s);" toTopOrToBottom stops
                yield "}"
            }
            |> Seq.iter (sb.AppendLine >> ignore)
            sb.ToString ()
        
let transformerToCssExpectations = [
    """<LinearGradientBrush StartPoint="0.5,0" EndPoint="0.5,1" xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"><LinearGradientBrush.GradientStops><GradientStop Color="#FF6699CC" Offset="0.33" /><GradientStop Color="#FF3A70A6" Offset="0.66" /><GradientStop Color="#FF1A4C80" Offset="1" /></LinearGradientBrush.GradientStops></LinearGradientBrush>""","""{
    background-image: linear-gradient(to bottom, #6699cc 33%, #3a70a6 66%, #1a4c80 100%);
}"""
    """<LinearGradientBrush StartPoint="0.5,0" EndPoint="0.5,1" xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"><LinearGradientBrush.GradientStops><GradientStop Color="#FF72A1D0" Offset="0.25" /><GradientStop Color="#FF7FAAD4" Offset="0.55" /><GradientStop Color="#FF8CB2D8" Offset="1" /></LinearGradientBrush.GradientStops></LinearGradientBrush>""","""{
    background-image: linear-gradient(to bottom, #72a1d0 25%, #7faad4 55%, #8cb2d8 100%);
}"""
]
open Transformers

transformerToCssExpectations
|> Seq.map (fun (x,y) -> y,  tryParseBrush x |> Option.map (fun x -> x.ToCss()))
|> fun x -> x.Dump("final result")
|> ignore