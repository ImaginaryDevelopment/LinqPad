<Query Kind="FSharpProgram" />

let checkmarks = // excellent info on an official unicode checkmark http://www.fileformat.info/info/unicode/char/2713/index.htm
    [
        // html entity hex strings I think?
        //http://www.alt-codes.net/check-mark-symbols.php
        // translation assistance from http://stackoverflow.com/a/13492748/57883
        
        "&#10003;"
        "&#10004;"
        "&#9989;"
        "&#9745;"
        "&#9083;"
        "&#128504;"
    ]
type CharacterMapping = {HtmlEntity: string; Decoded: string; Int:int list; Hex: string list}

checkmarks
|> Seq.map (fun x -> x, System.Net.WebUtility.HtmlDecode x)
|> Seq.map (fun (x,s) -> 
    let ints = s |> Seq.map int |> List.ofSeq
    {
        HtmlEntity=x
        Decoded=s
        Int= ints
        Hex= ints |> List.map (fun x -> x.ToString("X"))
    }
)
|> Dump
|> ignore

