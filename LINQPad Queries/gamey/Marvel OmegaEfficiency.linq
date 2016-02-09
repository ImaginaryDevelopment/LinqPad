<Query Kind="FSharpProgram" />

//marvel heroes omegas
open System
open System.Collections.Generic

#if INTERACTIVE //appears to function as desired in linqpad and .fsx
type System.Object with
    member x.Dump() = printfn "%A" x
    member x.Dump(s) = printfn "%s:%A" s x
#endif

let dumps (t:string) x = x.Dump(t)
let dumpt (t:string) x = 
    x.Dump(t)
    x
//type PointCost = | Cumulative of int | TaxedCumulative of int * int //with
    //static member carryTax  = 

type DamageType = | Physical | Energy | Mental with
    static member All = [DamageType.Physical; DamageType.Energy; DamageType.Mental] |> set
    static member IsAll dt =
        dt |> set |> Set.isSubset DamageType.All
    static member IsSubset dtAllowed dtPower =
        set dtAllowed |> Set.isSubset (set dtPower)

let (|AllDamageTypes|_|) dt =
    if DamageType.IsAll dt then Some () else None
let (|SubsetDamageType|_|) dt inputs =
    if DamageType.IsSubset inputs dt then Some() else None

type DamageTarget = |Melee |Ranged |All with
    static member IsRelevantTarget dtHeroFocus dtOmega = 
        match dtOmega with
        |Some DamageTarget.All -> true
        |Some DamageTarget.Melee -> dtHeroFocus = DamageTarget.Melee
        |Some DamageTarget.Ranged -> dtHeroFocus = DamageTarget.Ranged
        |None -> false

let (|AllDamageTargets|_|) dto =
    match dto with
    | Some dt -> if DamageTarget.All = dt then Some () else None
    | _ -> None

// DamageTarget None is for summoned only
type DamageLimitation= { DamageTypes:DamageType seq; DamageTargets:DamageTarget option;  (* Area:bool; *) IsSummoned:bool} with
    static member AllHero = {DamageTypes=DamageType.All; DamageTargets=Some DamageTarget.All; IsSummoned=false}
    static member Summoned = {DamageTypes=DamageType.All; DamageTargets=None; IsSummoned=true}
    //| DmgType of DamageType seq * DamageTarget
    //| DmgTarget of DamageTarget
    //| Area 
    //| Summoned

type Omega={PointCost:int;Dmg:int;Value:float;Level:int; DamageLimitation: DamageLimitation} with //TotalPointCost:int; TotalValue:float;
    member x.AsPercentage = float x.Dmg / float 40
    static member FromDamage dmgTypes fCost index dmg = 
        let pointCost= fCost index 
        {PointCost=pointCost; Dmg=dmg; Value= float dmg / float pointCost;Level=index + 1; DamageLimitation = dmgTypes}
    static member FromPercentage dmgTypes fCost index percent= Omega.FromDamage dmgTypes fCost index (percent * 40)
    static member FromCritDamage dmgTypes fCost critChance index critDmg = Omega.FromDamage dmgTypes fCost index (float critDmg * critChance |> int)
    static member FromFighting fCost index fighting = Omega.FromPercentage DamageLimitation.AllHero fCost index (fighting * 3)

type OmegaTotal = {TotalPointCost:int;TotalDmg:int;Tax:int; TotalValue:float; Level:int; DamageLimitation:DamageLimitation}

let totalCost (omegas:Omega seq,tax): OmegaTotal seq = 
    Seq.scan (fun omegaTotal omega -> 
        {omega with PointCost= omegaTotal.PointCost + omega.PointCost; Value = float omega.Dmg / float ( omegaTotal.PointCost + omega.PointCost)}
        ) {PointCost =0; Dmg =0; Value = 0.; Level = 0; DamageLimitation= {DamageTypes=DamageType.All; DamageTargets=Some DamageTarget.All; IsSummoned=false}} omegas
    |> Seq.skip 1
    |> Seq.map (fun o -> {TotalPointCost = o.PointCost+tax;TotalDmg = o.Dmg; Tax = tax; TotalValue = o.Value; Level = o.Level; DamageLimitation=o.DamageLimitation})

module physical = 
    let purePhysical = {DamageTypes=[DamageType.Physical]; DamageTargets= Some DamageTarget.All; (* Area=true; *) IsSummoned=false;}
    type Omega with
        static member FromStrength fCost index strength = Omega.FromPercentage purePhysical fCost index (strength*4)
    let superSoldierSerum= [1..5] |> Seq.mapi (Omega.FromStrength (fun i -> 200 + 10 * i)) 

    let reaverProgram = [1..10] |> Seq.mapi (Omega.FromPercentage purePhysical  (fun i -> 40 + 5 * i) )
    let exTechOp =[24..24..240] |> Seq.mapi (Omega.FromDamage purePhysical (fun i -> i*2 + 30))
    let showstopperAmmo = [18..18..360] |> Seq.mapi (Omega.FromDamage purePhysical (fun i -> i*2+18))

    let deathLokProgram =
            [12..12..72]@[83;96;107;120;132;144;155;167;180;192;204;215;227;240;251;264;275;288;300]
            |> Seq.mapi (Omega.FromDamage purePhysical (fun i -> i + 12) )

module energy = 
    let pureEnergy = {DamageTypes=[DamageType.Energy]; DamageTargets= Some DamageTarget.All; (* Area=true; *) IsSummoned=false;}
    let sunfire = [1..10] |> Seq.mapi (Omega.FromPercentage pureEnergy (fun i -> 40 + 5 * i))
    let bishop = [1..10] |> Seq.mapi (Omega.FromPercentage pureEnergy (fun i -> 35 + 5 * i))
    let exogenesis = [1..10] |> Seq.mapi (Omega.FromPercentage pureEnergy (fun i -> 50 + 2 * i))
    let doomsMysticalArmor = [6..6..120] |> Seq.mapi(Omega.FromDamage pureEnergy (fun i -> 16 + i))

module mental = 
    let pureMental = {DamageTypes=[DamageType.Mental]; DamageTargets = Some DamageTarget.All; IsSummoned = false;}
    let cerebro = [60..60..1200] |> Seq.mapi (Omega.FromDamage pureMental (fun i -> 20 + 2 * i))
    let zarathos = [6..6..120] |> Seq.mapi (Omega.FromDamage pureMental (fun i -> 16 + i))
    let masterTelepath = []
    
module melee = 
    let pureMelee = {DamageTypes=DamageType.All; DamageTargets= Some DamageTarget.Melee; (* Area=true; *) IsSummoned=false;}
    let pymsHybrid = [1..20] |> Seq.mapi (Omega.FromPercentage pureMelee (fun i -> 60 + 2 * i ))

module ranged = 
    let pureRanged = {DamageTypes=DamageType.All; DamageTargets= Some DamageTarget.Ranged; (* Area=true; *) IsSummoned=false;}
    let assaultRifles = [1..20] |> Seq.mapi(Omega.FromPercentage pureRanged (fun i-> 40 + 5 * i))
    let psionicCharge = [1..10] |> Seq.mapi(Omega.FromPercentage pureRanged (fun i -> 40 + 2 * i)),63 // tax

module all =
    
    let warpath = [1..10] |> Seq.mapi (Omega.FromFighting (fun i -> 180 + 10 * i))
    let spintech = [60..60..600] |> Seq.mapi (Omega.FromDamage DamageLimitation.AllHero (fun i -> 38 + i * 2))
    let tacticalNeuralImplant critChance = [30..300] |> Seq.mapi (Omega.FromCritDamage DamageLimitation.AllHero (fun i-> 40 + i*5) critChance)

type Omega with
    static member FromEnergy fCost index energy = Omega.FromPercentage {DamageTypes=[DamageType.Energy;DamageType.Mental]; DamageTargets = Some DamageTarget.All; IsSummoned=false} fCost index (energy * 4)

let antigenesis = [1..10] |> Seq.mapi (Omega.FromEnergy (fun i -> 250 + 5 * i))
let neurobotics = [24..24..240] |> Seq.mapi (Omega.FromDamage {DamageTypes=[DamageType.Physical;DamageType.Energy]; DamageTargets= Some DamageTarget.All; IsSummoned=false;}  (fun i -> i + 25)) 
let focusedPlasmaCannon = [ 8*5 ..40.. 160*5] |> Seq.mapi (Omega.FromDamage {DamageTypes=[DamageType.Physical;DamageType.Energy];DamageTargets =Some DamageTarget.Ranged; IsSummoned=false} (fun i -> i*2 + 30))    

let labelSeq label o = o|> Seq.map(fun o -> label,o)
type OmegaSeries = {Omegas: Omega seq; Label:string; Tax:int}

let allOmegas critChance = 
    let psionicCharge,psyTax = ranged.psionicCharge
    [
        {Omegas=physical.exTechOp;Label= "ExTechOp"; Tax=0}
        {Omegas=physical.superSoldierSerum;Label="SuperSoldierSerum" ; Tax=0}
        {Omegas=physical.showstopperAmmo;Label="ShowstopperAmmo"     ; Tax=45}
        {Omegas=physical.deathLokProgram;Label="Deathlok"            ; Tax=0}
        {Omegas=physical.reaverProgram;Label="Reaver"                ; Tax=0}

        {Omegas=energy.exogenesis;Label= "Exogenesis"                ; Tax=0}
        {Omegas=energy.sunfire;Label="Sunfire"                       ; Tax=0}
        {Omegas=energy.bishop;Label="Bishop"                         ; Tax=0}
        {Omegas=energy.doomsMysticalArmor;Label= "DoomsMysticalArmor"; Tax=0}
        
        {Omegas=mental.cerebro;Label="Cerebro"                       ; Tax=46}
        {Omegas=mental.zarathos; Label="Zarathos"                    ; Tax=3}
        {Omegas=antigenesis;Label= "Antigenesis"                     ; Tax=0}
        {Omegas=neurobotics ;Label="Neurobotics"                     ; Tax=0}
        {Omegas=focusedPlasmaCannon;Label= "FocusedPlasmaCannon"     ; Tax=0}

        {Omegas=melee.pymsHybrid;Label= "PymsHybrid"                 ; Tax=0}

        {Omegas=ranged.assaultRifles;Label= "AssaultRifles"          ; Tax=0}
        {Omegas=psionicCharge;Label="PsionicCharge"                  ; Tax = psyTax}

        {Omegas=all.spintech;Label= "Spintech"                       ; Tax=0}
        {Omegas=all.tacticalNeuralImplant critChance;Label= "TacticalNeuralImplant"; Tax=6}
        {Omegas=all.warpath;Label= "Warpath"; Tax=0}
    ]
    |> Seq.map (fun os -> 
        totalCost (os.Omegas,os.Tax)
        |> Seq.map(fun o -> o,os.Label)
        //|>  //|> Seq.map (LabeledOmega.FromOmega lbl))
        )
    |> Seq.collect id 
    |> Seq.sortBy (fun (o,_) -> -o.TotalValue)
 
let isRelevantDamage heroDamageTypes heroDamageTargets heroIsSummoner omegaDamageLimitation= 
    let isRelevantDamageType heroDamageTypes omegaDamageLimitation = 
        match omegaDamageLimitation.DamageTypes with
        | AllDamageTypes -> true
        | SubsetDamageType heroDamageTypes -> true
        | _ -> false
    let isRDType = isRelevantDamageType heroDamageTypes omegaDamageLimitation
    let isRDTarget = DamageTarget.IsRelevantTarget heroDamageTargets omegaDamageLimitation.DamageTargets
    omegaDamageLimitation.IsSummoned && heroIsSummoner || (isRDType && isRDTarget),isRDType,isRDTarget

// try taking walking down the efficiency
let optimize (omegas:(OmegaTotal*string) seq) points (heroDamageTypes:DamageType seq) (heroDamageTargets:DamageTarget) heroIsSummoner =
    let heroDamageTypes = heroDamageTypes |> set
    let map = Dictionary<string,OmegaTotal>()
    let excluded = Dictionary<string, DamageType seq* DamageTarget option * bool * bool>()
    let mutable points = points
    for (o,lbl) in omegas do
        let canAfford = o.TotalPointCost < points
        let isRelevantDamage,isRDType,isRDTarget = isRelevantDamage heroDamageTypes heroDamageTargets heroIsSummoner o.DamageLimitation
        if not isRelevantDamage then
            excluded.[lbl] <- (o.DamageLimitation.DamageTypes, o.DamageLimitation.DamageTargets,isRDType,isRDTarget)
        if isRelevantDamage && canAfford then
            let success,previousLevel = map.TryGetValue lbl
            map.[lbl] <- o
            if success then
                points <- points + previousLevel.TotalPointCost - o.TotalPointCost
            else
                points <- points - o.TotalPointCost
    map,points,excluded

module tests =
    let containsOmegaf lbl damageTypes damageTarget isSummoner f = 
        let included = 
            allOmegas 0.4 
            //|> Seq.filter (fun (o,_) -> isRelevantDamageType damageTypes damageTarget isSummoner o.DamageLimitation) 
            |> Seq.filter (fst >> (fun o -> o.DamageLimitation) >> isRelevantDamage damageTypes damageTarget isSummoner >> (fun (isRelevant,_,_) -> isRelevant)) 
            |> Seq.map snd
        if Seq.contains lbl included |> f then
            failwithf "%s should have been included in Summons=%A;%A,%A" lbl isSummoner damageTypes damageTarget

    let containsOmega lbl damageTypes damageTarget isSummoner = containsOmegaf lbl damageTypes damageTarget isSummoner not
    let excludesOmega lbl damageTypes damageTarget isSummoner = containsOmegaf lbl damageTypes damageTarget isSummoner id
    let run ()= 
        let includesShowstopperAmmo = containsOmega "ShowstopperAmmo"
        let excludesShowstopperAmmo = excludesOmega "ShowstopperAmmo"
        includesShowstopperAmmo [DamageType.Physical] DamageTarget.Ranged false
        includesShowstopperAmmo [DamageType.Physical] DamageTarget.Melee false
        includesShowstopperAmmo [DamageType.Physical;DamageType.Mental] DamageTarget.Melee false
        includesShowstopperAmmo (DamageType.All |> Set.toList) DamageTarget.Melee false
        excludesShowstopperAmmo  [DamageType.Energy] DamageTarget.Ranged false
        excludesShowstopperAmmo  [DamageType.Energy] DamageTarget.Melee false
        excludesShowstopperAmmo  [DamageType.Energy;DamageType.Mental] DamageTarget.Ranged false

        containsOmega "AssaultRifles" DamageType.All DamageTarget.Ranged false
        containsOmega 

let critRate,damageTypes,damageTargets =0.337, [Physical], Melee
let map,points,excluded = optimize (allOmegas critRate) 3219 damageTypes damageTargets false
map
|> dumpt (sprintf "map for %A;%A" damageTypes damageTargets)
|> ignore
points.Dump("leftover points")
excluded.Dump("excluded")