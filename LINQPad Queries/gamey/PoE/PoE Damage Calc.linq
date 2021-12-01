<Query Kind="FSharpProgram" />

// Phys damage calc jives with PoB up to a single hit
// conversions and attack speed not included yet

let debug = false
type Damage =
    |Phys
    |Lightning
    |Cold
    |Fire
    |Chaos
    with
        override x.ToString() = sprintf "%A" x
        member x.ToDump() = string x
type Option'<'t>(x:'t option) =
    member __.Value = x
    static member op_Implicit(x:Option'<'t>):Option<'t> = x.Value
    member x.ToDump() = sprintf "%A" x
    override x.ToString() = sprintf "%A" x.Value
   
let None' = Option'(None)   
let Some' x = Option'(Some x)
let (|Some'|None'|) (x:Option'<'t>) =
    match x.Value with
    | Some v -> Some' v
    | None -> None'
    
type NumberBase = decimal
let getEff base' inc more:NumberBase=
    let baseInc = (base'*(100m+inc)/100m)
    let total = baseInc * more
    if debug then
        (base',inc,more,baseInc,total).Dump("base',inc,more,baseInc,total")
    total
let getWeaponDmg minDmg maxDmg speed:NumberBase=
    (minDmg + maxDmg) / 2.0m * speed
    
//let combineMore items =
//    1.0::items
//    |> Seq.reduce(fun x y -> x * y)
//[
//    getTotalDamage 100.0 300.0 0.0
//    getTotalDamage 100.0 350.0 0.0
//    getTotalDamage 100.0 300.0 0.5
//    getTotalDamage 200.0 400.0 0.3
//]
//|> ignore

let foldMinMax (min,max) (mn,mx) =
    min+mn, max+mx
let foldDamage:seq<NumberBase*NumberBase> -> NumberBase*NumberBase =
    Seq.fold foldMinMax (LanguagePrimitives.GenericZero,LanguagePrimitives.GenericZero)
let filterType dt' =
    function
    // this value applies to all types
    |None,v -> Some v
    // this value applies only to the type we are interested in
    |Some dt,v when dt = dt' -> Some v
    | _ -> None
    
let foldDmgType type' :seq<Damage option*NumberBase> -> NumberBase =
    Seq.fold (fun t x ->
        match filterType type' x with
        | Some v -> t + v
        | None -> t
    ) (LanguagePrimitives.GenericZero)
    
let foldMore :seq<NumberBase> -> NumberBase =
    Seq.fold (fun t v ->
        if v > LanguagePrimitives.GenericOne then invalidOp "Bad more mod"
        t * (LanguagePrimitives.GenericOne + v)
    ) (LanguagePrimitives.GenericOne)
    >> fun v ->
        if debug then
            v.Dump("More folded")
        v
    
type DamageVector = {Type:Damage;Min:NumberBase;Max:NumberBase}
type Weapon =
    {Name:string; Speed:NumberBase; Damage:DamageVector list} with
        member x.GetDmg type' =
            x.Damage
            |> Seq.choose (function | d when d.Type=type' -> Some (d.Min,d.Max) | _ -> None)
            |> Seq.fold foldMinMax (LanguagePrimitives.GenericZero,LanguagePrimitives.GenericZero)
        member x.PhysDmg = 
            let mn,mx=
                x.Damage
                |> Seq.choose(function {Type=Phys;Min=mn;Max=mx} -> Some(mn,mx) | _ -> None)
                |> foldDamage
            getWeaponDmg mn mx x.Speed
type Skill = {Name:string; Eff:NumberBase}

type WeaponStyle =
    |One of Weapon
    |Two of Weapon*Weapon
    
//type WeaponSkill = {Skill:Skill; Weapon:Weapon} with
//    member x.Min = x.Weapon.Min * x.Weapon.Speed * x.Skill.Eff
type ModifierType = |More | Increase
[<NoComparison>]
type CharacterAttack = {WeaponStyle:WeaponStyle; Modifications:(ModifierType*Damage Option'*NumberBase) list;Skill:Skill; SpeedIncrease:NumberBase;SpeedMore:NumberBase;Adds:DamageVector list} with
    member x.Get1h() =
        match x.WeaponStyle with
        | One(w) -> w
        | Two(w,_) -> w
    member x.GetPhys (w:Weapon) =
        w.GetDmg Phys
        |> fun (mn,mx) -> 
            let eff = x.Skill.Eff
            x.Adds
            |> Seq.choose(function |{Type=Phys;Min=min;Max=max} -> Some(min,max) | _ -> None)
            |> foldDamage
            |> fun (min,max) ->
                mn*eff+min * eff,mx*eff+max*eff
    
    member x.Phys1hHit =
        x.Get1h()
        |> x.GetPhys
    member x.Phys2h =
        match x.WeaponStyle with
        | One _ -> None
        | Two (_,w2) -> Some<| x.GetPhys w2
    member x.Phsy1hHitTotal =
        let moar = x.Modifications |> Seq.choose(function |More,d,v -> Some(d.Value,v) | _ -> None) |> Seq.choose (filterType Phys) |> Seq.map(fun x -> x / 100m) |> foldMore
        let inc = x.Modifications |> Seq.choose(function |Increase,d,v -> Some (d.Value,v)| _ -> None) |> foldDmgType Phys
        
        let getEff v = getEff v inc moar
        let m,n = x.Phys1hHit
        let result =  getEff m, getEff n
        if debug then
            (moar,inc,m,n,result).Dump("m,inc,min,max,r")
        result
        
let entropyScratch = {Name="Entropy Scratch";Damage=[{Type=Phys;Min=109.0m;Max=310.0m}]; Speed=1.63m}
let dragonBarb = {Name="Dragon Barb";Damage=[{Type=Phys; Min=91.0m; Max=275.0m}];Speed=1.94m}
let empyreanScalpel= {Name="Empyrean Scalpel";Damage=[{Type=Phys;Min=71.0m;Max=164.0m}]; Speed=1.4m}
let lancingSteel = {Name="Lancing Steel"; Eff=0.81m}
let iveBeenImpaled = 
    {   WeaponStyle=Two(empyreanScalpel,dragonBarb)
        SpeedIncrease= 0.88m; SpeedMore= 0.1m;Skill=lancingSteel
        Modifications=[
                        Increase,Option' None, 145m
                        Increase,Option' <| Some Phys,276m
                        More, None', -37m
                        More, Some' Phys, 20m
                    
                    ]
        Adds=[
                {Type=Phys;Min=9.0m;Max=15.0m}
        ]
    }
    
iveBeenImpaled 
|> Dump
|> ignore