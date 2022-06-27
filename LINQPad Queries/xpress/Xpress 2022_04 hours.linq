<Query Kind="FSharpExpression" />

// calculate hours for PM 04-2022
let rate = 45.0m
let items =
    [
        "12", 
            (DateTime(2022,04,12,9,12,0) - DateTime(2022,04,12,8,36,0)).Minutes
            + (DateTime(2022,04,12,9,58,0) - DateTime(2022,04,12,9,12,0)).Minutes
        
        
        
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