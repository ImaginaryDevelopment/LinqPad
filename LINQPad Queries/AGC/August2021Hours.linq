<Query Kind="FSharpExpression" />

let rate = 75.0m
let items =
    [
        "9", 120
        "10", 3 * 60 + 4
        "11", 3 * 60 + 44 + 24 + 29
        "12", (3+1+2) * 60 + 26 + 16 + 59
        "16", 60
        "17", 59
        "18", 25 + 12
        "19", (3+1) * 60 + 3 + 20 + 48
        "23", 27
        "24", 26
        "25", 44 + 14
        "26", (3+2) * 60 + 54 + 34 + 16
        "27", (1+1+1) * 60 + 10 + 23 + 11
        "30", 2 * 60 + 30 + 3
    ]
    |> List.map(fun (dt,mins) ->
        let amount = Math.Round(float mins / 60.0,2)
        dt + "th",amount, decimal amount * rate
    )
items.Dump("lines")
(0.0m, items)
||> List.fold(fun total (_,_,x) ->
    total + x
)