//parse brush pasting from snoop

#if INTERACTIVE
#I @"C:\Users\Brandon\AppData\Local\LINQPad\NuGet.FW46\FParsec\FParsec.1.0.2\lib\net40-client\" // references AppData\local\ ...  since . is %localappdata%\TEMP
// #I 
#r "FParsecCS.dll"
#r @"FParsec.dll"
#endif

open FParsec

module Seq =
    let any items = items |> Seq.exists (fun _ -> true)

let sampleText = """<LinearGradientBrush StartPoint="0.5,0" EndPoint="0.5,1" xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"><LinearGradientBrush.GradientStops><GradientStop Color="#FF6699CC" Offset="0.33" /><GradientStop Color="#FF3A70A6" Offset="0.66" /><GradientStop Color="#FF1A4C80" Offset="1" /></LinearGradientBrush.GradientStops></LinearGradientBrush>"""

let test p str = 
    match run p str with
    |Success(result, _, _)   -> printfn "Success: %A" result
    |Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
type GStop = {Color:string;Offset:float}
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

    let attr = localName .>>. (pstring "=" >>. strLiteral)
    let lgbTag = 
        // assuming for now that startpoint and endpoint are always in the same order
        pstring "<LinearGradientBrush" >>. opt (spaces >>. point) .>>. opt (spaces >>. point) .>> spaces .>> skipMany attr .>> (pstring ">") 
        |>> fun (startp,endp) -> {StartPoint = startp; EndPoint=endp;Stops = list.Empty}
    let selfcloser p = p .>> spaces .>> (pstring "/>")
    let color = 
        let colorStarter = fun c -> isAsciiLetter c || isDigit c
        let colorCont = colorStarter
        optional (pstring "#") >>. (identifier (IdentifierOptions(isAsciiIdStart=colorStarter, isAsciiIdContinue= colorCont)))
    let gradientStop = pstringCI "<GradientStop" >>. spaces >>. attrNamed "Color" color .>>. (spaces >>. attrNamed "Offset" pfloat .>> spaces .>> (skipString "/>")) |>> (fun s -> {Color=fst s; Offset=snd s})
    let lgbWithStops =
        lgbTag .>>. (pstringCI "<LinearGradientBrush.GradientStops>" >>. many gradientStop)
    lgbWithStops

// test (pstring "<") sampleText
test parseBrush sampleText

let tryParseBrush text = 
    match run parseBrush sampleText with
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
                if Seq.any x.Stops then
                    yield "["
                    yield! x.Stops |> Seq.map(fun gStop -> sprintf "    GradientStop(argbFromHex \"#%s\",%f)" gStop.Color gStop.Offset) // doesn't account for proper F# formatting of a list
                    yield "]"
                    yield sprintf "|> Seq.iter gb.GradientStops.Add"
                }
            |> Seq.iter (sb.AppendLine >> ignore)
            sb.ToString ()

open Transformers
tryParseBrush sampleText 
|> Option.bind (fun x -> x.ToFSharp () |> Some)
|> printfn "%A"
