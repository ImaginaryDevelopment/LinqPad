<Query Kind="FSharpProgram" />

let dump title (x:'t) =
    x.Dump(description=title)
    x
    
let formatMoney (x:obj) = String.Format("{0:C}",x)
let formatFloat (x:float) = String.Format("{0:#.##E+0}",x)
let formatBig (x:int64) =  String.Format("{0:#.##E+0}",x)

let snip1 (x:string) =
    x.[0 .. x.Length - 2]
let (|EndsWith|_|) (d:string) (x:string) =
    if x.EndsWith(d) then Some(snip1 x) else None
let parse (x:string) =
    try
        let fMult factor = decimal >> (*) factor
        match x with
        | EndsWith "T" x -> x |> fMult 1_000_000_000_000m
        | EndsWith "B" x -> x |> fMult 1_000_000_000m
        | EndsWith "m" x -> x |> fMult 1_000_000m
        | EndsWith "k" x -> x |> fMult 1_000m
        | _ -> x |> decimal
    with _ ->
        x.Dump("fail")
        reraise()
type Efficiency = | Eff of float
    with
        member x.ToDump() =
            match x with
            | Eff x -> formatFloat x
type Analysis = {
    Name: string
    Efficiency: Efficiency
    Dmg: string
    Cost: string
}
        

let getEfficiency (name,increase:int64,cost:int) =
    let eff = float increase / float cost
    {   Name= name
        Efficiency = Eff eff
        Dmg = formatBig increase
        Cost = formatMoney cost
    }
    
type LevelType =
    | MToM of level:int*increase:int*cost:int
    | Custom of cost:int

let getCost =
    function
    | Custom x -> x
    | MToM(_,_,x) -> x
    
let costTables = [
    (1, 24, 1_980)
    (25, 25, 7_730)
    (50, 25, 0)
]
    
DateTime.Now.Dump()
[
    "Fire", [
        //"Octapyre", "128.02 m", 30_110
        //"Fire Devil", "545.72", 90_860
        //"Burnerbunn", "171.33", 30_110
        //"Fryangle", "15.99", 7_730
        "BurnestBunn", "5.16E+17", 1_230_480
        "Lord Firefly", "585.56T", 46_730
    ]
    "Other", [
        "Spoutest", "1.95E+17", 1_140_860
        "Tortuoise", "917.50T", 184_230
        "Conduit", "5.48T", 1_980
        "Ginga", "2.47T", 1_980
        //"Chicken Pea", "73.6", 30_110
        //"Sea Mustang", "313.81", 66_980
        //"Leafier", "106.27", 30_110
        //"Sheldon", "22.11", 7_730
    ]
]
|> List.map(fun (cat,items) ->
    cat, items
    |> List.map(fun (n,i,c) -> n, parse i |> int64, c)
    |> List.map getEfficiency
    |> List.sortByDescending(fun x -> x.Efficiency)
)
|> Dump
|> ignore