<Query Kind="FSharpProgram" />

//marvel heroes omegas

type Omega={PointCost:int;Dmg:int;Value:float;Level:int} with //TotalPointCost:int; TotalValue:float;
    member x.AsPercentage = float x.Dmg / float 40
    static member FromDamage fCost index dmg = 
        let pointCost = fCost index 
        {PointCost=pointCost; Dmg=dmg; Value= float dmg / float pointCost;Level=index + 1}
    static member FromPercentage fCost index percent= Omega.FromDamage fCost index (percent * 40)

let totalCost (omegas:Omega seq) = 
    Seq.scan (fun omegaTotal omega -> 
        {omega with PointCost= omegaTotal.PointCost + omega.PointCost; Value = float omega.Dmg / float ( omegaTotal.PointCost + omega.PointCost)}
        ) {PointCost =0; Dmg =0; Value = 0.; Level = 0} omegas
    |> Seq.skip 1
    
module physical = 
    type Omega with
        static member FromStrength fCost index strength = Omega.FromPercentage fCost index (strength*4)
    let deathLokProgram = [12..12..72]@[83;96;107;120;132;144;155;167;180;192;204;215;227;240;251;264;275;288;300] |> Seq.mapi (Omega.FromDamage (fun i -> i + 12))
    let reaverProgram = [1..10] |> Seq.mapi (Omega.FromPercentage (fun i -> 40 + 5 * i))
    let neurobotics = [24..24..240] |> Seq.mapi (Omega.FromDamage (fun i -> i + 25)) 
    let exTechOp =[24..24..240] |> Seq.mapi (Omega.FromDamage (fun i -> i*2 + 30))
    let superSoldierSerum= [1..5] |> Seq.mapi (Omega.FromStrength (fun i -> 200 + 10 * i)) 
    superSoldierSerum.Dump()
    let showstopperAmmo = [18..18..360] |> Seq.mapi (Omega.FromDamage (fun i -> i*2+18))
    //let focusedPlasmaCannon = 
    
module all =
    type Omega with
        static member FromFighting fCost index fighting = Omega.FromPercentage fCost index (fighting * 3)
    let warpath = [1..10] |> Seq.mapi (Omega.FromFighting (fun i -> 180 + 10 * i))
    let spintech = [60..60..600] |> Seq.mapi (Omega.FromDamage (fun i -> 38 + i * 2))
    
type LabeledOmega = {PointCost:int;Dmg:int;Value:float;Level:int;Display:string} with
    static member FromOmega label (o:Omega) = {LabeledOmega.PointCost = o.PointCost;Dmg=o.Dmg; Value=o.Value;Level=o.Level;Display = label}

let rec omegaCombinations (availableOmegas : LabeledOmega seq) = 
    seq{
        for (omegaName,omegas) in availableOmegas |> Seq.groupBy(fun o -> o.Display) do
            //printfn "omegas %A" omegas
            let remaining = availableOmegas |> Seq.filter (fun (phy:LabeledOmega) -> phy.Display <> omegaName)
            for omega in omegas do
                if remaining <> Seq.empty then
                    for combo in omegaCombinations remaining do
                        yield omega::combo
        }

let labelSeq label o = o|> Seq.map(fun o -> label,o)
let physicals = 
    [ 
        physical.deathLokProgram,"Deathlok"
        physical.reaverProgram,"Reaver"
        physical.neurobotics ,"Neurobotics"
        physical.exTechOp, "ExTechOp"
        physical.superSoldierSerum,"superSoldierSerum" 
        physical.showstopperAmmo, "showstopperAmmo"
        all.spintech, "spintech"
        all.warpath, "warpath"
    ]
    |> Seq.map (fun (v,lbl) ->  v |> totalCost |> Seq.map (LabeledOmega.FromOmega lbl))
    
let allPhysicals =
    physicals
    |> Seq.collect id
    |> Seq.sortBy (fun o -> -o.Value)
    //|> printfn "%A"

omegaCombinations allPhysicals
|> Seq.maxBy(fun combo -> combo |> Seq.sumBy (fun (o:LabeledOmega) -> o.Dmg))
|> printfn "%A"