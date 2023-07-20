<Query Kind="FSharpProgram" />

let initPoints = 1649
let memoryAttributes = 10
type Attr =
    | Intelligence
    | Insight
    | Spellcraft
    | Wisdom
    | Dominance
    | Patience
    | Mastery
    | Empathy
    | Versatility
type StatLayout =
    { 
        int : int
        ins : int
        spc : int
        wis : int
        dom : int
        pat : int
        mas : int
        emp : int
        ver : int
    } with
        static member Zero = {int = 0; ins = 0; spc = 0; wis=0; dom=0;pat=0;mas=0;emp=0;ver=0}
        static member SetInt v x = {x with int = v}
        static member SetIns v x = {x with ins = v}
        static member SetSpc v x = {x with spc = v}
        static member SetWis v x = {x with wis = v}
        static member SetDom v x = {x with dom = v}
        static member SetPat v x = {x with pat = v}
        static member SetMas v x = {x with mas = v}
        static member SetEmp v x = {x with emp = v}
        static member GetTotal x = x.int + x.ins + x.spc + x.wis + x.dom + x.pat + x.mas + x.emp + x.ver
        static member SetAttr stat v x =
            match stat with
            | Intelligence -> StatLayout.SetInt v x
            | Insight -> StatLayout.SetIns v x
            | Spellcraft -> StatLayout.SetSpc v x
            | Wisdom -> StatLayout.SetWis v x
            | Dominance -> StatLayout.SetDom v x
            | Patience -> StatLayout.SetPat v x
            | Mastery -> StatLayout.SetMas v x
            | Empathy -> StatLayout.SetEmp v x
            | Versatility -> { x with ver = v}
        static member GetAttr stat v =
            match stat with
            | Intelligence -> v.int
            | Insight -> v.ins
            | Spellcraft -> v.spc
            | Wisdom -> v.wis
            | Dominance -> v.dom
            | Patience -> v.pat
            | Mastery -> v.mas
            | Empathy -> v.emp
            | Versatility -> v.ver
        member x.Total = StatLayout.GetTotal x
            
let validateSpending title previousOpt x =    
    let total = StatLayout.GetTotal x
    if total > initPoints then
        let text = sprintf "Failing: more points spent then available %i - %i" total initPoints
        match previousOpt with
        | None -> 
            x.Dump(text)
        | Some p ->
            (p,x).Dump(text)
        failwithf "invalid state: %s" title
type GType =
    | Exact
    | Spend
    
let items = [
    Spend, Intelligence, 150
    Spend, Empathy, 100
    Spend, Mastery, 250
    Spend, Spellcraft, 200
    Exact, Patience, 100 // 5
    Spend, Intelligence, 200
    Spend, Spellcraft, 250
    Exact, Insight, 50
    Spend, Dominance, 250
    Exact, Empathy, 175 // 10
    Exact, Intelligence, 250
    Exact, Wisdom, 150
    Exact, Wisdom, 200
    Spend, Empathy, 250
    Exact, Wisdom, 250 // 15
    Exact, Insight, 150 // 16
    Exact, Insight, 200
    Exact, Insight, 250
    Exact, Patience, 150
    Exact, Patience, 250
    Spend, Intelligence, 250 + memoryAttributes
    Spend, Versatility, 250 + memoryAttributes
]

((StatLayout.Zero,initPoints),items)
||> Seq.fold(fun (stats,points) (gt, s, v) ->
    let current = StatLayout.GetAttr s stats + memoryAttributes
    let cost = v - current
    if cost < 0 then
        (stats,points,gt,s,v).Dump("failing")
        failwithf "Cost less than current %A %i - %i" s current cost
    elif cost = 0 then
        (stats, points)
    else
        match gt, cost > points with
        | Exact, false
        | Spend, false ->
            let next = stats |> StatLayout.SetAttr s (v - memoryAttributes)
            validateSpending (sprintf "Spending %A - %i" s v) (Some stats) next
            (next, points - cost)
        | Spend, true ->
            let next = stats |> StatLayout.SetAttr s (points + StatLayout.GetAttr s stats)
            validateSpending (sprintf "Buying %A - %i" s v) (Some stats) next
            (next, 0)
            
        | Exact, true ->
            (stats,points)
)
|> fun (stat, pts) ->
    validateSpending "Final" None stat
    stat,pts
    //let total = StatLayout.GetTotal stat
    //
    //if total > points then
    //    stat.Dump(sprintf "Failing: more points spend then available %i - %i" total points)
    //    failwithf "%i pts left" pts
|> Dump
|> ignore