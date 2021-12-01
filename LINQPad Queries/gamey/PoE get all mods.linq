<Query Kind="FSharpProgram">
  <Reference Relative="..\..\..\PathOfSupporting\PoS\lib\net462\PathOfSupporting.dll">C:\projects\PathOfSupporting\PoS\lib\net462\PathOfSupporting.dll</Reference>
  <NuGetReference>FSharp.Data</NuGetReference>
  <Namespace>FSharp.Data</Namespace>
</Query>

// Get all possible mods for all item types
module Option =
    let ofTrue v b = if b then Some v else None
//type Simple = JsonProvider<""" { "name":"John", "age":94 } """>
//let simple = Simple.Parse(""" { "name":"Tomas", "age":4 } """)
[<Literal>]
let fp = @"C:\projects\poeaffix.github.io\mods.json" // from https://github.com/brather1ng/RePoE/blob/master/data/mods.json
// somewhat of a map at https://github.com/PoESkillTree/PoESkillTree/blob/master/PoESkillTree.GameModel/Items/ItemClass.cs#L64
//type ModJp = JsonProvider<ResolutionFolder= @"C:\projects\poeaffix.github.io", Sample="mods.json">
//type ModJp = JsonProvider<Sample=fp>
//let modJp = ModJp.Load fp
[<NoComparison>]
type MappedMod = {Name:string;Spawn_Weights:(string*int) seq;Raw:JsonValue}
let toMapped name (x:JsonValue) = 
    {Name=name;Raw=x;Spawn_Weights = x.GetProperty "spawn_weights" |> fun x -> x.AsArray() |> Seq.map(fun x -> x.GetProperty("tag").AsString(),x.GetProperty("weight").AsInteger())}
[<NoComparison>]
type GenerationTypeGroup = {GenerationType:string;Items :MappedMod seq}
[<NoComparison>]
type DomainContainer={Domain:string;GtGroups: GenerationTypeGroup seq}
let (|IsRing|_|) ({Spawn_Weights=x} as item) =
    x
    |> Seq.exists(fst >> (=) "ring")
    |> Option.ofTrue item
let domainGtGroupsLens f x = {x with GtGroups= f x.GtGroups}
let gtItemsLens f x = {x with Items=f x.Items}
let domainItemsLens f x = domainGtGroupsLens (Seq.map (gtItemsLens f)) x
let isRingOrUnique gt =
    function
    | IsRing x -> Some x
    | x -> if gt = "unique" then Some x else None
let chooseItems f x =
    x
    |> Seq.map(fun d -> {d with GtGroups=d.GtGroups |> Seq.map(fun gtg -> {gtg with Items= gtg.Items |> Seq.choose (f d.Domain gtg.GenerationType) })})
    


let info = JsonValue.Load fp
info.Properties()
|> Seq.groupBy(snd >> fun x -> x.GetProperty "domain" |>fun p -> p.AsString())
|> Seq.map(fun (x,items) -> x,items |> Seq.groupBy(snd >> fun x -> x.GetProperty "generation_type"))
|> Seq.map(fun (x,gtGroups) ->
    {   Domain=x
        GtGroups=   gtGroups
                    |> Seq.map(fun (gt,items) -> {GenerationType=gt.AsString();Items=items|> Seq.map (fun (name,v) -> toMapped name v )})})

// begin slimming for exploration
//|> chooseItems (fun _d gtg mm ->
//    match mm with
//    | IsRing _ -> Some mm
//    | _ -> if gtg = "unique" && mm.Spawn_Weights.Any() then Some mm else None
//)
//|> fun x -> x.Dump(maximumDepth=2)

|> ignore