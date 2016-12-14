<Query Kind="FSharpExpression" />


let mil = 1000000L
let bil = mil * 100L
let creations = 
    [
        "clone", 0L
        "light", 0L
        "stone", 0L
        "soil", 4050L
        "air", 8000L
        "water", 28250L
        "plant", 77000L
        "tree", 12200L
        "fish", 693000L
        // unknown digits will be multipliers
        "animal", 323465L * mil / 1000L
        "human", 706712L * mil / 1000L
        "river", 141366L
    ]
creations
|> Seq.map (fun (name, cost) -> name, cost, cost * 2L)
//|> Dump
//|> ignore