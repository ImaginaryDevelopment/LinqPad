<Query Kind="FSharpExpression" />

let isValid (text:string) = 
    (Ok 0,text.ToCharArray()) ||> Seq.fold(fun state c ->
        match state,c with
        | Error v, _ -> Error v
        | Ok v, '(' -> v + 1 |> Ok
        | Ok v, ')' when v > 0 -> v - 1 |> Ok
        | Ok v, ')' when v <= 0 -> Error "too many closes"
        | _ -> state
    )
    |> Result.bind(fun v -> if v = 0 then Ok() else Error <| sprintf "Parens imbalance %i" v)
[
    "()", true
    "(()", false
    "(())", true
    "(()())", true
    "()((()()))", true
    "())", false
]
|> Seq.map(fun (text,valid) ->
    isValid text |> sprintf "%A", valid,text
)