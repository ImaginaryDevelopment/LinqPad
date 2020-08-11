<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Core</NuGetReference>
  <NuGetReference>FSharp.Data</NuGetReference>
  <Namespace>FSharp.Data</Namespace>
  <Namespace>Microsoft.FSharp.Core</Namespace>
</Query>

// minecraft pokecube catch rate
let (|ParsedInt|_|) (x:string) = 
    match int.TryParse x with
    | true, v -> Some v
    | _ -> None
type Status = 
    | Freeze
    | Sleep

type PokeDb = CsvProvider< "https://raw.githubusercontent.com/Thutmose/PokecubeDatabase/master/databases/baseStats.csv", HasHeaders = true>
let pokeDb = PokeDb.Load("https://raw.githubusercontent.com/Thutmose/PokecubeDatabase/master/databases/baseStats.csv")
type PokeMob = { Id:int;Name:string; CaptureRate: int option} 
    with 
        member x.getMaxHealth () = 1.
        member x.getHealth () = 0.5
        member x.getStatus():Status option = None
        member x.getCatchRate() = 245.
let pokeMap = 
    pokeDb.Rows
    |> Seq.map (fun r ->
        r.Number,{ Id =r.Number; Name = r.Name; CaptureRate = r.``Capture Rate`` |> Option.ofNullable}
    )
    |> Map.ofSeq
    

type Pokecube = {Id:int;Type:string} with
    member x.getCaptureModifier pokemob pokecubeId = 
        1.5
// https://github.com/Thutmose/Pokecube/blob/10b613c83174c8938032b48a4d8c6b02de92b66b/Pokecube%20Core/src/main/java/pokecube/core/utils/Tools.java
let getCatchRate hpMax hp catchRate cubeBonus statusBonus =  
    ((3. * hpMax - 2. * hp) * catchRate * cubeBonus * statusBonus) / (3. * hpMax)
module PokecubeItems = 
    let getFilledCube x : Pokecube option = None
// computeCatchRate in src
let computeCatchRateFromBonus (pokemob:PokeMob) cubeBonus = 
    let hpMax = pokemob.getMaxHealth()
    let r = Random()
    let hp = pokemob.getHealth()
    let statusBonus = 
    
        match pokemob.getStatus() with
        | Some Freeze
        | Some Sleep ->
            2.
        | Some _ ->
            1.5
        | None -> 1.
    let catchRate = pokemob.getCatchRate()
    let a = getCatchRate hpMax hp catchRate cubeBonus statusBonus
    if a > 255. then
        5
    else
        let b = 1048560. / Math.Sqrt(Math.Sqrt(16711680. / a))
        let mutable n = 0
        if r.Next(65535) <= int b then
            n <- n + 1
        if r.Next(65535) <= int b then
            n <- n + 1
        if r.Next(65535) <= int b then
            n <- n + 1            
        if r.Next(65535) <= int b then
            n <- n + 1
        n
let computeCatchRate pokemob pokecubeId =
    PokecubeItems.getFilledCube pokecubeId
    |> Option.map (fun cb ->
        cb.getCaptureModifier pokemob pokecubeId
    )
    |> Option.getOrDefault 0.
    |> computeCatchRateFromBonus pokemob

let suggestions = 
    [
        for KeyValue(k,v) in pokeMap do
            yield string k
            yield v.Name
        
    ]
    
Util.ReadLine("Pokemon?", "561", suggestions)
//pokeMap.[561]
|> fun x -> computeCatchRateFromBonus x 1.5
|> function
    |ParsedInt x -> pokeMap.[x]
    | x -> pokeMap |> Seq.map (function |KeyValue(k,v) -> v) |> Seq.find(fun p -> p.Name = x)
    
|> Dump
|> ignore