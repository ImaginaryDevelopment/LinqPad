<Query Kind="FSharpProgram" />

// see which denominations should be converted up for sale
[<Measure>] type Plain
[<Measure>] type Enchanted
[<Measure>] type EBlock

let pe = 32m* 5m<Plain/Enchanted>
let ebe = 32m* 5m<Enchanted/EBlock>
let dumpt title x = x.Dump(description=title); x
let formatMoney (x:obj) = String.Format("{0:n0}",x)
type UnitType =
    |Plain of decimal<1/Plain>
    |Ench of decimal<1/Enchanted>
    |EBlock of decimal<1/EBlock>
    with
        member x.Raw =
            match x with
            | Plain x -> decimal x
            | Ench x -> decimal x
            | EBlock x -> decimal x
let values = [
    "Plain", Plain 7.4m<1/Plain>
    "E", Ench 1281.6m<1/Enchanted>
    "EB", EBlock <| (decimal 207_402) * 1m<1/EBlock>
]

let format (x:obj) = String.Format("{0:n0}",x)

values
|> Seq.map(fun (txt,value) ->
    let plainValue = 
        match value with
        | Plain x -> x
        | Ench x -> x / pe
        | EBlock x -> x / ebe /pe
        
    sprintf "%s -> $%f ($%s)" txt plainValue (format <| box value.Raw)
)
|> dumpt "cost options"
|> ignore
