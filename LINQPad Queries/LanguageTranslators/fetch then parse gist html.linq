<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>FParsec</NuGetReference>
  <NuGetReference>FSharp.Data</NuGetReference>
  <Namespace>FParsec</Namespace>
</Query>

// pull out gist embed html for usage without .js requirement
let hoist f x = f x; x
let flip f y x = f x y
module Seq =
    let contains x = Seq.exists(fun item -> item = x)

let replace delimiter (replacement:string) (text:string) = 
    text.Replace(delimiter,replacement)
let split (delimiters:string seq) (s:string) = s.Split(delimiters |> Array.ofSeq, StringSplitOptions.None)
let splitLines = split [ "\r\n"; "\n" ]

let escapeHtml = 
    replace "<" "&lt;"
    >> replace ">" "&gt;"

let url = "https://gist.github.com/ImaginaryDevelopment/4a735576a7f7349b4bdf7ba345e5bbb7.js"
let useCache = true

let dumpReverse :  (obj) -> unit =
    let dc = DumpContainer()
    dc.Dump() |> ignore
    (fun o -> 
        match dc.Content with
        | :? (obj list) as items -> List.Cons(o,items)
        | _ -> [ o ]
        |> fun content -> dc.Content <- content
    )

let dumpFailureText = 
    string
    >> splitLines
    >> List.ofArray
    >> function 
        | errorLocation:: sampleText:: pointer :: rest -> 
            errorLocation.Dump()
            sampleText
            |> escapeHtml
            |> flip (sprintf "<pre style=\"font-family: monospace\">%s\r\n%s</pre>") pointer
            |> Util.RawHtml
            |> Dump
            |> ignore
            rest.Dump()
        | x -> x.Dump("Achievement unlocked: Alanis Morissette Inception. Failed to parse failure text")
            
let dumpFailure = 
    function 
    | Failure (x,_,_) -> 
        dumpFailureText x
    | Success (_,_,_) -> ()
    
let testf p x = 
    run p x
    |> dumpFailure
    
module Parsing = 
    let test p str = 
        match run p str with
        |Success(result, _, _)   -> printfn "Success: %A" result
        |Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
        
    // should work for either ' or " or possible more
    let makeLiteralParser quoteType (ignoredEscapes:(char *(unit -> string)) seq) = 
        let pEscapedAnyOf =
            ignoredEscapes 
            |> Seq.map (fst >> string) 
            |> delimit String.Empty
            |> sprintf "\\%cnrt%s" quoteType 
        let normalCharSnippet = manySatisfy (fun c -> 
            c <> '\\' && c <> quoteType)    
        let escapedChar = 
            pstring "\\"
            >>. (anyOf pEscapedAnyOf
                |>> function
                    | 'n' -> "\n"
                    | 'r' -> "\r"
                    | 't' -> "\t"
                    | x when ignoredEscapes |> Seq.map fst |> Seq.contains x -> 
                        let f = ignoredEscapes |> Seq.find (fun (c,_) -> c = x) |> snd
                        f()
                    | c   -> string c)
        between (pstring (string quoteType)) (pstring (string quoteType)) 
            (stringsSepBy normalCharSnippet escapedChar)

    let trailingNewOrSpaces = optional (newline .>> spaces)
    let pwriteText = 
        makeLiteralParser '\'' [
            '"', fun () -> "\""
            '/', fun () -> "/"
            ]
    let parseWrite = 
        between (pstring "document.write(") (pstring ")") pwriteText 
        .>> trailingNewOrSpaces
        |> many1 

module Html = 
    open FSharp.Data
    
    type HtmlNode with
        static member cssSelect selector (hn:HtmlNode) = hn.CssSelect selector
    type FSharp.Collections.List<'T> with
        static member getItem i (array: 'T list) = array.[i]
    type GetFilesResult = 
        |Single of string
        |Multiple of (string*string) list
        
    let getFiles text = 
        let doc = 
            sprintf "<html><head></head><body>%s</body></html>" text
            |> HtmlDocument.Parse
        let wrapFile = sprintf "<div class=\"gist\">%s</div>"
        let files = doc.CssSelect ".gist-file"
        let getName = 
            HtmlNode.cssSelect ".gist-meta"
            >> Seq.head
            >> HtmlNode.cssSelect "a"
            >> List.getItem 1
            >> HtmlNode.innerText
            
        match files with
        | []
        | [_] -> Single text
        | _::_ -> 
            files 
            |> List.map (fun f -> getName f, f |> string |> wrapFile)
            |> Multiple

let getGist () = 
    let getGist() : string = 
        use client = new System.Net.Http.HttpClient()
        Async.RunSynchronously(async{
            return client.GetStringAsync(url)
        }).Result 
    if useCache then
        let result = Util.Cache((fun _ -> getGist()), "gist.js text")
        result
    else 
        getGist()
    

open Parsing    

testf pwriteText "'<hello'"
testf parseWrite "document.write('<a')"
testf parseWrite "document.write('<div id=\"gist43513509\" class=\"gist\">\n    <div class=\"gist')"

let gist = getGist()

gist
//|> test parseWrite
|> run parseWrite
|> function 
    | Success (result,y,z) -> 
        match result with
        | [css; html] -> 
            dumpReverse css |> ignore
            Util.OnDemand("full html block", fun () -> html) |> dumpReverse |> ignore  
            match Html.getFiles html with
            | Html.GetFilesResult.Single html -> ()
            | Html.GetFilesResult.Multiple files -> 
                files |> Seq.map (fun (filename, text) -> filename, Util.OnDemand(filename, fun () -> text)) |> dumpReverse |> ignore    
        | _ -> result.Dump()
    | Failure (x,_,_) -> x |> dumpFailureText 