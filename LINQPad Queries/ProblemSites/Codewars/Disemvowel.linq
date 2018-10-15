<Query Kind="FSharpProgram" />

let disemvowel s =
    s
    |> Seq.choose(function | 'a' | 'A' |'e'|'E'|'i'|'I'|'o'|'O'|'u'|'U' -> None | x -> Some x)
    |> Array.ofSeq
    |> String
    
disemvowel "hello world"
|> Dump
|> ignore
    
