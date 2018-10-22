<Query Kind="FSharpExpression" />

let evaporator (content: double) (evapPerDay: double) (threshold: double): int =
    let threshold = threshold * 0.01
    let startingPercentage = 1.0
    startingPercentage
    |> Seq.unfold(fun percentage ->
        let remainingPercentage = percentage - (percentage * evapPerDay * 0.01)
        if remainingPercentage >= threshold then
            Some (remainingPercentage,remainingPercentage)
        else None
    )
    |> Seq.append [startingPercentage]
    |> Seq.length


[  10.0, 10.0, 10.0, 22
]
|> List.map(fun (c,e,t,expect) ->
    evaporator c e t,expect
)
