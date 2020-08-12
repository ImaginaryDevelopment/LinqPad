<Query Kind="FSharpProgram" />

[<Measure>] type Diamond
[<Measure>] type EDiamond
//[<Measure>] type DBlock
[<Measure>] type EDBlock

let conversion = 32*5
module EDiamond =
    let toD (x:int<EDiamond>):int64<Diamond> =
        int64 x
        |> Checked.(*) (int64 conversion)
        |> (*) 1L<Diamond>
    let fromD(x:int<Diamond>):int<EDiamond> =
        int x / conversion * 1<EDiamond>
module EDBlock =
    let toED (x:int<EDBlock>):int<EDiamond>= 
        int x * conversion * 1<EDiamond>
        
    let fromED(x:int<EDiamond>):int<EDBlock>= 
        int x / conversion * 1<EDBlock>
    

type DiamondTotal = {D:int64<Diamond>; ED:int<EDiamond>; EDB:int<EDBlock>}

let inline zero< ^T when ^T : (static member Zero : ^T) > : ^T  = LanguagePrimitives.GenericZero
let dumpt title x = x.Dump(description=title); x
let formatCost (x:obj) = String.Format("{0:n0}",x)
    
let getCost edb =
    let ed = edb |> EDBlock.toED
    {EDB=edb;ED = ed; D= EDiamond.toD ed}
    
let enchblocksReq = 24<EDBlock>
let armorCost =
    enchblocksReq     
    |> getCost
    |> dumpt "diamonds Required"

let getDiamondTotal {D=d;EDB=edb;ED=ed} =
    let edbTotal = edb |> EDBlock.toED |> EDiamond.toD
    let edTotal = ed |> EDiamond.toD
    d + edTotal + edbTotal
//getDiamondTotal {D=0;EDB=1;ED=1}
//|> Dump
let getDiamondCost marketCost amountNeeded =
    marketCost * amountNeeded

let auctionEDBCost = 5_200_000 / 24
let auctionEDCost = 130_000 / 64
let costs = [
    // AH estimated, not literal
    "AH EDB", auctionEDBCost * 1<EDBlock> |> EDBlock.toED |> EDiamond.toD
    "AH ED", auctionEDCost * 1<EDiamond> |> EDiamond.toD
    "B EDB", 210_836 * 1<EDBlock> |> EDBlock.toED |> EDiamond.toD
    "B ED", 1_293 * 1<EDiamond> |> EDiamond.toD
    "B D", 9L<Diamond>
]
let owned = {EDB=zero;ED= 64<EDiamond> * 4 + 60<EDiamond>;D=zero}
costs
|> Seq.map(fun (txt,cost) ->
    sprintf "%s -> $%s" txt <| formatCost (cost * armorCost.D)
)
|> dumpt "cost options"
costs
|> Seq.minBy(fun (_,cost) -> cost)
|> fun (txt,cost) -> 
    let needed = armorCost.D - getDiamondTotal owned
    needed * cost
    |> formatCost
    |> sprintf "%s -> $%s" txt
|> dumpt "min cost"
|> ignore