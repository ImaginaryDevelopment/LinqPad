<Query Kind="FSharpProgram" />

//#if !INTERACTIVE
//#r "FSharp.Core"
//#endif
// open Microsoft.FSharp.Data.UnitSystems.SI
[<Measure>] type kg
[<Measure>] type km
[<Measure>] type Mm
[<Measure>] type Gm
[<Measure>] type g
type Weight = float<kg>
let commandPodWt = 200.<kg>
let toG = 1<g> / 1_000<kg>
let toMm x = x * 1.0<Mm> / 1000.0<km>
//let toGm x = x * 1.0<Gm> / 1000.0<Mm>

type OxModule = |LiquidOx |SolidOx
type RocketModule =
    |OxModule of OxModule
    |FuelModule
    |Cargo // any cargo type
    |Pod // Research or Passenger
type Fuel = 
    // | Steam
    | Petroleum
    // | Hydrogen
    // | LiquidHydrogen
type Oxydizer = | Oxylite | LiquidOxygen
type Thruster =
    // | Steam
    | Petroleum
    // | Hydro
type WeightSource = | ThrustMod of Thruster | Mod of RocketModule | Fuel of Weight
let getDryWeight : _ -> Weight =
    function
    |ThrustMod Petroleum -> 200.<kg>
    // |ThrustMod Hydro -> 500.<kg>
    |Mod FuelModule -> 100.<kg>
    |Mod (OxModule _)-> 100.<kg>
    |Mod Cargo -> 2000.<kg>
    |Mod Pod -> 200.<kg>
    |Fuel x -> x

let getRawFuelDistance : (Fuel*Weight -> float<km>) =
    function
    // | Steam,x -> 20.0<km/kg> * x
    | Fuel.Petroleum,x -> 40.0<km/kg> * x
let getOxMult: _ -> float =
    function
    |Oxylite -> 1.0
    |LiquidOxygen -> 1.33
    
let getTotalThrust (f,fwt) (ox,owt):float<km> =
    let usableFuels = min (float fwt) (float owt) * 1.0<kg>
    let raw =  getRawFuelDistance (f,usableFuels)
    raw |> (*) (getOxMult ox)

let getPenalty weight : Weight =
    let wt1 = (weight / 300.0<kg>) ** 3.1793
    printfn "wt=%A;wt^=%A" weight wt1
    wt1
    |> max (weight / 1.0<kg>) |> ((*) 1.0<kg>)
type Rocket = {
    Thruster:Thruster
    Mods:RocketModule list
    Fuel:Fuel*Weight
    Oxydizer:Oxydizer*Weight
    AddedWeight:Weight // you can get wrong elements in your fuel tank
    }
// does not include capsule
let getRocketDryWeight =
    function
    |{Thruster = t; Mods=m} ->
        let twt = getDryWeight (ThrustMod t)
        let mwt = m |> List.map (WeightSource.Mod>>getDryWeight) |> List.sum
        twt + mwt
let getWetWeight : Rocket -> Weight =
    function 
    |{Fuel=(_,fwt);Oxydizer=(_,owt)} -> fwt+owt

// types added to amount to ensure consistency (can't have two types of oxy I think)// different oxy takes different storage tank types
type FuelStorage = {FuelStore:Weight;OxydizerStore:Weight*Oxydizer option}
type FuelAmount = {Fuel:Fuel*Weight;Oxydizer:(Weight*Oxydizer)}
let nostorage = {FuelStore=0.0<kg>;OxydizerStore=0.0<kg>,None}
()

let getStorage : RocketModule -> FuelStorage =
    let ox x = {FuelStore= 0.0<kg>;OxydizerStore= 2700.0<kg>,Some x}
    function
    |OxModule LiquidOx -> ox Oxydizer.LiquidOxygen
    |OxModule SolidOx -> ox Oxydizer.Oxylite
    |Cargo -> nostorage
    |Pod -> nostorage
    |FuelModule _ -> {nostorage with FuelStore= 900.0<kg>}
()
let (|Ox|NoOx|) =
    function
    | 0.0<kg>, (Some x) -> failwithf "Ox store claims type without value (%A)" x
    | 0.0<kg>, None -> NoOx
    | x, None -> failwithf "Ox store says %A but had no type" x
    | _ -> Ox
let addOxStorage o1 o2 =
    match o1,o2 with
    | NoOx,NoOx -> 0.0<kg>, None
    | Ox, NoOx -> o1
    | NoOx, Ox & o2 -> o2
    | (Ox & (_,Some ot1)), (Ox & (_,Some ot2)) when ot1 <> ot2 -> failwithf "Conflicting Oxydizers %A <> %A" ot1 ot2
    | NoOx,(x,Some ot2) ->
        if x <= 0.0<kg> then failwithf "Invalid ox store, weight without type"
        x, Some ot2
    | (x,Some ot1),NoOx ->
        if x <= 0.0<kg> then failwithf "Invalid ox store, weight without type"
        x, Some ot1
    | (o1,Some ot),(o2, Some _) ->
        o1+o2, Some ot
    | x -> failwithf "Case %A was not accounted for" x
    
let (|HasOxStore|NoOxStore|) =
    function
    |{OxydizerStore= Ox _} -> HasOxStore
    |{OxydizerStore= NoOx} -> NoOxStore
    |{OxydizerStore= 0.0<kg>,Some _} -> failwithf "Ox store claims type without value"
    |{OxydizerStore= x,_} when x > 0.0<kg> -> HasOxStore
    
let (|HasFuelStore|NoFuelStore|) = function |{FuelStore=0.0<kg>} -> NoFuelStore | _ -> HasFuelStore
let addStorage store x: FuelStorage =
    match store, getStorage x with
    | _, NoOxStore & NoFuelStore -> store
    | {OxydizerStore= _,Some ot1}, {OxydizerStore= _,Some ot2} when ot1 <> ot2 -> failwithf "Conflicting Oxydizers %A <> %A" ot1 ot2
    | {FuelStore=fs1;OxydizerStore=os1},{FuelStore=fs2;OxydizerStore=os2} ->
        let os = addOxStorage os1 os2
        {FuelStore=fs1+fs2;OxydizerStore=os}
let getTotalStorage (mods:RocketModule list): FuelStorage = (nostorage,mods) ||> List.fold addStorage

// let validateStorage (fs:FuelStorage) (f:FuelAmount) =// match fs
type RocketView = { Rocket:Rocket
                    FuelStorage:FuelStorage
                    DryWeight:Weight
                    WetWeight: Weight
                    TotalWeight:Weight
                    Penalty:Weight
                    TotalThrust: float<Mm>
                    Range:float<Mm>}
//
// check the combination of modules can hold the fuel/oxy
// // for now maybe forever, mixed oxydizers is a fail
// // no fuel storage is invalid
let getRange =
    function
    |{Thruster=t;Mods=mods;Fuel=fs;Oxydizer=os;AddedWeight=aw} as r ->
        let storage:FuelStorage = getTotalStorage mods 
        let dry = getRocketDryWeight r + commandPodWt
        let wet = getWetWeight r 
        let wt = dry + wet + aw 
        let penalty = getPenalty wt 
        let rawrange = getTotalThrust fs os 
        let range = float rawrange - float penalty 
        match fs,t with 
        | (Fuel.Petroleum, _), Thruster.Petroleum -> 
            {   Rocket= r 
                FuelStorage= storage 
                Penalty= penalty 
                WetWeight= wet 
                DryWeight= dry 
                TotalWeight= wt 
                TotalThrust= rawrange |> toMm // |> toGm
                Range= range * 1.0<km> |> toMm 
            }
let tests= // : Rocket list =
    [
        // expected: 50_000km, penalty: -9793km, // dryweight: ~2.8t, // wet: ~2.5t
        {
            Thruster=Thruster.Petroleum
            Mods=[OxModule SolidOx;OxModule SolidOx;FuelModule;Pod;Cargo]
            Fuel=Fuel.Petroleum,1250.0<kg>
            Oxydizer=Oxylite,1250.0<kg>
            AddedWeight=0.0<kg>
        }
        // expected: 10_054km (10.054Mm), penalty: -3266km, dryweight: ~2.6t, wet: ~0.7t
        {
            Thruster=Thruster.Petroleum
            Mods=[OxModule SolidOx;FuelModule;Cargo]
            Fuel=Fuel.Petroleum,333.0<kg>
            Oxydizer=Oxylite,333.0<kg>
            AddedWeight=0.0<kg>
        }
    ] |> List.map getRange
printfn "Values %A" tests
tests
|> Dump