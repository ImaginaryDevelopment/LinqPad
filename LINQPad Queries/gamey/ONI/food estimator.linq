<Query Kind="FSharpProgram" />

let normaldupes,bottomless = 5,0
let cook = true
let singleEggModel = true // one egg then kill, or multiples?

[<Measure>] type cycle
[<Measure>] type second
[<Measure>] type calorie
type TimeType =
    | Cycle of float<cycle>
    | Second of float<second>
    
type Domestication =
    | Wild
    | Dom
    
// reference data at https://oni-db.com/
type Plant = 
    | BogBucket
    | BristleBlossom
    | Mealwood
    
type Critter =
    | Hatch
    | Drecko
    
type EdibleType =
    | Plant of Plant
    | Critter of Critter
    
type CritterInfo = { 
    // how long is it a baby? will not produce eggs while baby
    Incubate: float<cycle>
    Baby: float<cycle>
    // not modeling glum for now, brain full
    //Glum: bool
    //Lullabied: bool
    Reproduction: float<cycle>
    Life: float<cycle>
}

// lifecycle = plant production rate, or animal incubation + baby + reproduction + kill? time?
// perhaps if it can reproduce multiple times in one life we should alter formula?
type EdibleInfo = {LifeCycle:float<cycle>; CIOpt: CritterInfo option}
type Edible = { Type: EdibleType; CaloriesPer:int<calorie>; DomInfo: EdibleInfo; WildInfo: EdibleInfo}

let getEdibleInfoByDom x =
    function
    | Dom -> x.DomInfo
    | Wild -> x.WildInfo
    
let getCritterLifeCycle {Baby=b; Incubate=i;Reproduction=r} =
    if singleEggModel then
    // baby -> adult -> egg
        b + i + r
    else
        r
    

let inline floatt<[<Measure>]'t> (x:int<'t>) : float<'t> = float x |> LanguagePrimitives.FloatWithMeasure


    
module Data =    
    let getPlantInfo =
        let createEI lc = {LifeCycle = lc;CIOpt = None}
        function
        | BogBucket ->
            Some {Type = Plant BogBucket; CaloriesPer=1840<calorie>; DomInfo = createEI 6.6<cycle>; WildInfo = createEI 26.4<cycle>} 
        | BristleBlossom ->
            Some { Type = Plant BristleBlossom; CaloriesPer=1600<calorie>; DomInfo = createEI 6.<cycle>; WildInfo = createEI 24.<cycle>}
            
        
    let getCritterInfo =
        function
        | Hatch -> // https://oxygennotincluded.fandom.com/wiki/Hatch?so=search
            // Happy Hatches can lay up to 16 eggs over their entire adult life (95 days)
            let wild =
                let w =
                    {
                        Incubate = 20.<cycle>
                        Baby = 5.<cycle> // ???
                        Reproduction = 60.<cycle>
                        Life = 100.<cycle>
                    }
                let lc = getCritterLifeCycle w
                {LifeCycle=lc; CIOpt = Some w}
            let dom =
                let d = 
                    {
                        Incubate = 20.<cycle>
                        Baby = 0.<cycle> // ???
                        Reproduction = 5.88<cycle>
                        Life = 100.<cycle>
                    }
                let lc = getCritterLifeCycle d
                {LifeCycle=lc; CIOpt = Some d}
            Some { Type = Critter Hatch; CaloriesPer = 3200<calorie>; WildInfo = wild; DomInfo = dom }
        
    
let lookupEdibleInfo =
    function
    | Plant p -> Data.getPlantInfo p
    | Critter c -> Data.getCritterInfo c
    
let targetInfo, targetDomType =
    let t = Critter Hatch, Dom
    lookupEdibleInfo (fst t)
    |> function
        | None -> failwithf "Could not get info for %A" t
        | Some x -> x, snd t
// oversimplified, missing recipes
let maybeCook =
    function
    | Plant BogBucket -> Some 2240<calorie>
    | Plant Mealwood -> Some 1700<calorie>
    | Critter Hatch -> Some 4000<calorie> // needs 2 I think to become 4000<calorie>
    | Critter Drecko -> Some 4000<calorie>
    | _ -> None
    
let caloriesPerCycle (v:Edible) domType =
    let ei = getEdibleInfoByDom v domType
    match maybeCook v.Type with
    | Some c ->
        c, ei.LifeCycle
    | None ->
        v.CaloriesPer, ei.LifeCycle
    |> fun (cal,lc) ->
        let cpc = floatt cal / lc
        cpc

let dupeNeed = 
    let normalneeds = 1000<calorie/cycle>
    normaldupes *  normalneeds
    + bottomless * (normalneeds + 500<_>)

let sourcesNeeded =
    // try up to x sources
    [1..200]
    |> List.map(fun i -> 
        let c = float i * caloriesPerCycle targetInfo targetDomType
        c > floatt dupeNeed,(i,c)
    )
    |> List.tryFind fst
    |> Option.map snd
    
printfn $"Normal: %i{normaldupes}, Bottomless: %i{bottomless}"
sourcesNeeded
|> function
    | None -> printfn "Could not find enough food to feed your litter"
    | Some (plantCount,calories) ->
        printfn $"%i{plantCount} %A{targetInfo.Type}(s) would produce %.2f{calories} per cycle"
//|> Dump
|> ignore