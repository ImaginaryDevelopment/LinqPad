<Query Kind="FSharpExpression" />

let formatN n v = sprintf "%.1f %s(s)" v n
let formatElapsed (ts:TimeSpan) =
    [
        1, "second"
        60, "minute"
        60 * 60, "hour"
        60 * 60 * 24, "day"
    ]
    |> List.rev
    |> List.choose(fun (i,name) ->
        let seconds = Math.Abs(ts.TotalSeconds)
        let i = float i
        if seconds >= i || i <= 1 then
            
            let v = if i > 0 then seconds / i else seconds
            formatN name v
            |> Some
        else None
    )
[
    TimeSpan.FromSeconds 0
    TimeSpan.FromSeconds 1
    TimeSpan.FromSeconds 61
    TimeSpan.FromMinutes(2.)
    61 * 60 |> TimeSpan.FromSeconds
    TimeSpan.FromHours(1.)
]
|> List.map(fun ts ->
    ts, formatElapsed ts
)