<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
</Query>

// TODO: elemental damage with attacks option
let getText () = System.Windows.Forms.Clipboard.GetText()
let useClipboardCache = false
let flip f x y = f y x
let maybeCache useCache keyOpt f = 
    match useCache,keyOpt with
    |true, None -> Util.Cache(Func<_>(f))
    |true, Some k -> Util.Cache(Func<_>(f),k)
    | false, _ -> f()
let (|RMatch|_|) (p:string) (x:string) = 
    let r = Regex.Match(x,p)
    if r.Success then
        Some r
    else None
let (|RMatches|_|) (p:string) (x:string) = 
    let r = Regex.Matches(x,p)
    if r.Count > 0 then
        Some r
    else None

let dmgPattern = 
    let start = @"(\w+) Damage:\s*"
    let numbers = @"(\d+-\d+)"
    let augOpt = @"\s*(?:\(augmented\)\s*,?)?\s*"
    let numRepeat = sprintf "(%s%s)+" numbers augOpt
    sprintf "%s%s" start numRepeat
let (|AttackSpeed|_|) = 
    function
    |RMatch "Attacks per Second\s*:\s*(\d+\.\d+)" rAps -> rAps.Groups.[1].Value |> float |> Some
    |_ -> None
let (|ApsSpecial|_|) =
    function
    |RMatch "(\d+)% increased Attack Speed while a Rare or Unique Enemy is Nearby" rAps -> rAps.Groups.[1].Value |> float |> Some
    |_ -> None
let (|CritChance|_|) =
    function
    | RMatch "Critical Hit Chance: (\d+\.\d+)%" rCrit -> rCrit.Groups[1].Value |> float |> Some
    | _ -> None
let (|CritBonus|_|) =
    function
    | RMatch "\\+(\d+)% to Critical Damage Bonus" rCb -> rCb.Groups[1].Value |> float |> Some
    | _ -> None
let getDamageEntries =    
    function
    | RMatches dmgPattern rDmg -> 
        rDmg 
        |> Seq.cast<Match> 
        
        |> Seq.map(fun m -> 
            m.Groups.[1].Value, 
                m.Groups.[3].Captures 
                |> Seq.cast<Capture> 
                |> Seq.map(fun c -> 
                    c.Value.Split('-') 
                    |> Seq.map float
                    |> List.ofSeq
                )
                |> List.ofSeq
        )
        |> List.ofSeq
        |> Some
    | _ -> None
    
let (|DamageEntries|_|) = 
    let result =
        getDamageEntries
        >> Option.map(List.map(fun (t,values) ->
                t,values
                |> List.map(List.sum >> flip(/) 2.)
                |> List.sum
            
            )
        )
    result
            
let calc' aps dmgs =
    let indiv =
        dmgs
        |> Seq.map(fun (t,v) -> t, v * aps)
        |> List.ofSeq
    indiv |> List.map snd |> Seq.sum, indiv
// what if the character has crit chance or bonus nodes?
let calcCrit aps (crit,critBonus) dmgs =
    if crit < 0.0 || crit > 99.0 then failwith $"Invalid crit chance '%f{crit}'"
    let indiv =
        dmgs
        |> Seq.map(fun (t,v) ->
            let dmg = v * aps
            let dmgChance = 100.0 - crit
            let critDmg = 
                let defaultCritBonus = 100.0
                critBonus
                |> Option.map ((+) defaultCritBonus)
                |> Option.defaultValue defaultCritBonus
            //(dmg, dmgChance, critDmg, crit).Dump("crit info")
            let effDmg = ((dmgChance * dmg) + (crit * dmg * (1.0 + critDmg / 100.0))) / 100.0
                
            t, effDmg
        )
        |> List.ofSeq
    indiv |> List.map snd |> Seq.sum, indiv
    
type WeaponDps = {Dps:float; DpsTypes : (string*float) list}

let calcDps (aps:float) aps2Opt dmgs critOpt: (string * float) list = 
    let total,individualDamages = calc' aps dmgs
    match critOpt with
    | None ->
        printfn "dps calculated total %.2f" total
        [
            yield! individualDamages@["Total", total]
            match aps2Opt with
            | Some aps' ->
                let rAps = aps * (1.+aps' / 100.)
                let t2, i2 = calc' rAps dmgs
                let i2 = i2 |> List.map(fun (n,x) -> sprintf "%s2" n, x)
                printfn "dps with Rare or Unique Nearby %.2f" t2
                yield! i2 @["Total2", t2]
                
            | _ -> ()
        ] 
    | Some (crit,critBonus) ->
        let totalCrit, indiCrit = calcCrit aps (crit,critBonus) dmgs
        indiCrit@["TotalCrit", totalCrit]
    
let dpsLineTestCases=[
    "Elemental Damage: 29-53 (augmented), 4-92 (augmented)", (29+53)/2 + (4+92)/2
]
dpsLineTestCases
|> Seq.iter(fun (text,expected) ->
    match text with
    | DamageEntries dmgs ->
        if text.StartsWith("Elemental Damage") then
            dmgs
            |> Seq.tryFind(fun (t,_) -> t = "Elemental")
            |> function
                | None -> failwithf "elemental not detected"
                | Some (_,v) ->
                
                    if v <> float expected then
//                        (text,getDamageEntries text).Dump("fails")
                        failwithf "Actual %A, expected %i" v expected
    | x -> failwith $"Damage entry unrecognized: '%s{text}'"
        
)
maybeCache useClipboardCache None getText
//|> fun x -> x.SplitLines()
|> fun x ->
    match x with
//    | AttackSpeed aps & DamageEntries dmgs & CritChance crit ->
////        (aps,dmgs).Dump("debug")
//        let aps2Opt = match x with ApsSpecial aps' -> Some aps' | _ -> None
//        let dps = calcDps aps aps2Opt dmgs (Some (crit,None))
//        printfn "Aps:%A, dps: %A" aps dps
//        ()
    | AttackSpeed aps & DamageEntries dmgs ->
//        (aps,dmgs).Dump("debug")
        let aps2Opt = match x with ApsSpecial aps' -> Some aps' | _ -> None
        let dps: (string * float) list = calcDps aps aps2Opt dmgs None
        printfn "Aps:%A, dps: %A" aps dps
        
        match x with
        | CritChance crit ->
            let cb = 
                match x with
                | CritBonus cb -> Some cb
                | _ -> None
            let critDps = calcDps aps aps2Opt dmgs (Some (crit,cb))
            printfn "crit dps: %A" critDps
        | _ -> ()
        ()
    | AttackSpeed _ -> printfn "Couldn't find damage"
    | DamageEntries _ ->
        printfn "Could find AttackSpeed"
    | _ -> printfn "Couldn't find either"

    printfn ""
    printfn ""

// notes about previous league estimations
    printfn "Poe1: For level 50ish we found a ~245 dagger without difficulty"
    printfn "Poe1: For endgamish ~ 320 1h"
    x.Dump("Weapon")