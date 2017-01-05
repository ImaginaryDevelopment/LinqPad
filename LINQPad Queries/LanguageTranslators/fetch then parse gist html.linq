<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <NuGetReference>FParsec</NuGetReference>
  <NuGetReference>FSharp.Data</NuGetReference>
  <Namespace>FParsec</Namespace>
</Query>

// pull out gist embed html for usage without .js requirement
// instead of <!-- <script src="https://gist.github.com/ImaginaryDevelopment/4a735576a7f7349b4bdf7ba345e5bbb7.js"></script> --></div>
// or <script src="https://raw.github.com/moski/gist-Blogger/master/public/gistLoader.js" type="text/javascript"></script>
module Seq =
    let contains x items = items |> Seq.exists(fun item -> item = x)
    
let dumpReverse :  (obj) -> unit =
    let dc = DumpContainer()
    dc.Dump() |> ignore
    (fun o -> 
        match dc.Content with
        | :? (obj list) as items -> List.Cons(o,items)
        | _ -> [ o ]
        |> fun content -> dc.Content <- content
    )    
let replace delimiter (replacement:string) (text:string) = 
    text.Replace(delimiter,replacement)
let escapeHtml text = 
    text
    |> replace "<" "&lt;"
    |> replace ">" "&gt;"
let dumpFailureText f = 
    f 
    |> string
    |> splitLines
    |> List.ofArray
    |> function 
        | errorLocation:: sampleText:: pointer :: rest -> 
            errorLocation.Dump()
            sampleText
            |> escapeHtml
            |> fun x -> sprintf "<pre style=\"font-family: monospace\">%s\r\n%s</pre>" x pointer
            |> Util.RawHtml
            |> Dump
            |> ignore
            rest.Dump()
        | x -> x.Dump("failed to parse failure text")
let url = "https://gist.github.com/ImaginaryDevelopment/4a735576a7f7349b4bdf7ba345e5bbb7.js"
let useCache = true
let split (delimiters:string seq) (s:string) = s.Split(delimiters |> Array.ofSeq, StringSplitOptions.None)
let splitLines (s:string) = s |> split [ "\r\n"; "\n" ]
let dumpLine i text = 
    splitLines text |> Seq.skip (i - 1) |> Seq.take 1 |> Dump |> ignore
    text
let dumpLineLines i text = 
    splitLines text 
    |> Seq.skip (i-1) 
    |> Seq.take 1 
    |> Seq.map (fun s -> s.Replace("\\n", "\n").Replace("\\r","\r") |> splitLines)
    |> Dump
    |> ignore
    text
let dumpLineLine i j text = 
    splitLines text 
    |> Seq.skip (i-1) 
    |> Seq.take 1 
    |> Seq.collect splitLines
    |> Seq.skip (j-1)
    |> Dump
    |> ignore
    text
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
            sprintf "\\%cnrt%s" quoteType (ignoredEscapes |> Seq.map (fst >> string) |> delimit String.Empty)
        let normalCharSnippet = manySatisfy (fun c -> 
            //c.Dump("is normal?")
            c <> '\\' && c <> quoteType)    
        let escapedChar = 
            pstring "\\"
            >>. (anyOf pEscapedAnyOf // ignoredescapes should be for this use case:  \"\/
                |>> function
                    | 'n' -> "\n"
                    | 'r' -> "\r"
                    | 't' -> "\t"
//                    | '"' -> "\""
//                    | '/' -> "\/"
                    | x when ignoredEscapes |> Seq.map fst |> Seq.contains x -> 
                        let f = ignoredEscapes |> Seq.find (fun (c,_) -> c = x) |> snd
                        f() //sprintf "\%c" x
                    | c   -> string c)
        between (pstring (string quoteType)) (pstring (string quoteType)) 
            (stringsSepBy normalCharSnippet escapedChar)

    let trailingNewOrSpaces = optional (newline .>> spaces)
    let pwriteText = 
        //pstring "'" >>. manySatisfy (fun c -> c <> '\'') .>> pstring "'"
        makeLiteralParser '\'' [
            '"', fun () -> "\""
            '/', fun () -> "/"
            ]
    let parseWrite = 
        many1 (
            between (pstring "document.write(") (pstring ")") pwriteText .>> trailingNewOrSpaces
            )
//    test parseWrite "document.write(a)"
//    |> Dump
module Html = 
    open FSharp.Data
    //type GistProvider = HtmlProvider<"
    let getFiles text = 
        let doc = 
            sprintf "<html><head></head><body>%s</body></html>" text
            |> HtmlDocument.Parse
        let wrapFile = sprintf "<div class=\"gist\">%s</div>"
        let files = doc.CssSelect ".gist-file"
        files.Length.Dump("files")
        files |> dumpReverse |> ignore
        match files with
        | [] -> [text]
        | [_] -> [text]
        | _::_ -> 
            "in files branch!".Dump()
            files |> List.map (string>>wrapFile) // split this after testing
        
let getGist() = 
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
let splitGistFiles (text:string) = 
    Html.getFiles text

open Parsing    

testf pwriteText "'<hello'"
testf parseWrite "document.write('<a')"
testf parseWrite "document.write('<div id=\"gist43513509\" class=\"gist\">\n    <div class=\"gist')"
let gist = getGist()
gist
//|> dumpLineLines 2
|> splitLines
//|> Seq.skip 1
|> delimit "\r\n"
//|> test parseWrite
|> run parseWrite
|> function 
    | Success (result,y,z) -> 
        match result with
        | [css; html] -> 
            dumpReverse css |> ignore
            Util.OnDemand("full html block", fun () -> html) |> dumpReverse |> ignore
            splitGistFiles html
            |> dumpReverse
            |> ignore
        | _ -> result.Dump()
    | Failure (x,_,_) -> x |> dumpFailureText //.Dump("gist parse failure")