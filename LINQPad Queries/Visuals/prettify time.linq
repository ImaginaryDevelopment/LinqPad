<Query Kind="FSharpExpression" />

// see also Humanizer package

let minute, hour, day, week =
    let m = 60.0
    let h = m * 60.0
    let d = h * 24.0
    m,h,d,d * 7.0
let testers = [
    "no time", 0.0
    "few seconds", 3.0
    "a minute", minute
    "over min", minute + 1.
    "minutes bare", minute * 3.
    "minutes+", minute + 2.
    "hours bare", hour * 2.
    "hours+", hour + minute
    "almost day", day - 1.
    "days bare", day
]

let timeUnits =
    [
        "second", Some 60.0
        "minute", Some 60.0
        "hour", Some 24.0
        "day", Some 7.0
        "week", None
    ]
    
let prettyPattern amount =
    let pluralizeIf x single plural =
        if x >= 1.0 && x <= 2.0 then single else plural

    ((List.empty,amount), timeUnits |> List.indexed)
    ||> List.fold(fun (pretties,amount) (i, (period, toNext)) ->
        match toNext with
        | None ->
            if amount > 0.0 then
                let prettyPeriod = pluralizeIf amount period (period + "s")
                (amount,$"%.0f{amount} {prettyPeriod}")::pretties,0.0
            else pretties, 0.0
        | Some cutoff ->
            if amount > 0.0 || i = 0 then
                //printfn "Running some cutoff for %s - %A" period amount
                let amountNextUnit = Math.Truncate(amount / cutoff)
                let amountThisUnit = amount % cutoff
                let prettyPeriod = pluralizeIf amountThisUnit period (period + "s")
                (amountThisUnit, $"%.0f{amountThisUnit} {prettyPeriod}")::pretties, amountNextUnit
            else pretties, 0.0
    )
    |> fst
    |> function
        | [] -> failwith "uh why?"
        | h::[] -> [h]
        | x -> x |> List.filter(fun (v,_) -> v <> 0)
    
testers
|> List.rev
|> List.map(fun (t,v) -> t, prettyPattern v, v)