<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
</Query>

let getText () = System.Windows.Forms.Clipboard.GetText()
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

type WeaponDps = {Dps:float; DpsTypes : (string*float) seq}
let calcDps (aps:float) dmgs = 
    let x = 
        dmgs
        |> Seq.map(fun (t,v) -> t, v * aps)
        |> List.ofSeq
    let total = 
        x
        |> List.map snd
        |> Seq.sum
    printfn "dps calculated total %f" total
    x@["Total", total]
maybeCache false None getText
//|> fun x -> x.SplitLines()
|> fun x ->
    let dmgPattern = 
        let start = @"(\w+) Damage:\s*"
        let numbers = @"(\d+-\d+)"
        let augOpt = @"\s*(?!\(augmented\)\s*,?)?\s*"
        let numRepeat = sprintf "(%s%s)+" numbers augOpt
        sprintf "%s%s" start numRepeat
    let (|AttackSpeed|_|) = 
        function
        |RMatch "Attacks per Second\s*:\s*(\d+\.\d+)" rAps -> rAps.Groups.[1].Value |> float |> Some
        |_ -> None
    let (|DamageEntries|_|) = 
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
                        |> Seq.sum
                        |> flip (/) 2.
                    )
                    |> Seq.sum
            )
            |> Some
        | _ -> None
    match x, x with
    | AttackSpeed aps,DamageEntries dmgs ->
        let dps = dmgs |> calcDps aps
        printfn "Aps:%A, dps: %A" aps dps
        ()
    | AttackSpeed _, _ -> printfn "Couldn't find damage"
    | _, DamageEntries _ ->
        printfn "Could find AttackSpeed"
    | x -> printfn "Couldn't find either"
    printfn "For level 50ish we found a ~245 dagger without difficulty"
    printfn "For endgamish ~ 320 1h"
    [x].Dump("Weapon")