<Query Kind="FSharpProgram" />

//50 dark essence per 3 shards

[<Measure>] type Material
[<Measure>] type DarkEss
[<Measure>] type CosmicEss
[<Measure>] type EarthEss
[<Measure>] type LeafEss
[<Measure>] type WaterEss

// allows remainders, ugh
//let getDark cosmic earth leaf water =
//    let darks = [int cosmic; int earth; int leaf; int water] |> Seq.min
//    darks * 1<DarkEss>, cosmic - (darks*1<CosmicEss>), earth - (darks*1<EarthEss>), leaf - (darks*1<LeafEss>), water - (darks*1<WaterEss>)

   
let cosmicCost = 5.<CosmicEss>/40.<Material>
let earthCost = 5.<EarthEss>/40.<Material>
let leafCost = 5.<LeafEss>/100.<Material>
let waterCost = 5.<WaterEss>/100.<Material>

let getDarkMaterialCost () =
    6.<CosmicEss> / cosmicCost + 6.<EarthEss> / earthCost + 6.<LeafEss> / leafCost + 6.<WaterEss> / waterCost
    
    
    


// 200?
//let essCost = darkCost * (40 + 40 + 100 + 100) / 5

getDarkMaterialCost() 
|> printfn "1 Dark Essence Material Cost: %.0f (Not including cheese)"
|> Dump
|> ignore
