<Query Kind="FSharpProgram" />

[<Measure>]
type T
[<Measure>]
type B
[<Measure>]
type AA
[<Measure>]
type BB
[<Measure>]
type CC

//let million = 1000000uL
//let mil = million
//let billion = 1000uL * mil
//let B = billion
//let trillion = 1000uL * B
//let T = trillion
//let aa = 1000uL * T
//let bb = 1000uL * aa
//let cc = 1000uL * bb
[<Measure>]
type Dps

let aaToBB (aa:float<AA>) = aa / 1000.<AA> * 1.<BB>
let bbToCC (bb:float<BB>) = bb / 1000.<BB> * 1.<CC>

let ccToBB (cc:float<CC>) = cc * 1000.<BB> / 1.<CC>
let bbToAA (bb:float<BB>) = bb * 1000.<AA> / 1.<BB>
let inline display (x:float) (t:string) = x.ToString("C").Dump(t)
//million.ToString("C").Dump()

//display billion

//display bb
//let floatToULong (f:float) (places:uint64) = Convert.ToUInt64( f * 100. ) * places / 100uL

type Upgrade = { Name:string; Cost:float<CC>; Dps: float<BB>; CostPerDamage:float}

let costPerDps<[<Measure>]'a> (money:float<'a>) (dps:float<'a>) =
	money / dps
	
let playerUpgrade = 
	let cost,damage = 5.45<CC>, 20.80<AA>
	let bb = 5.45<CC> |> ccToBB 
	let damageBB:float<BB> = aaToBB damage
	{Name= "Player"; Cost= cost; Dps = damageBB; CostPerDamage = costPerDps bb  damageBB}
let peter =
	let bb, damage = 303.122<CC> |> ccToBB, 85.58<BB>
	let cost = bbToCC bb
	{Name = "Peter"; Cost = cost; Dps = damage; CostPerDamage = costPerDps bb damage}
let mikey =
	let bb = 14.88<CC> |> ccToBB
	bb / 3.83<BB>

display playerUpgrade.CostPerDamage playerUpgrade.Name
display peter.CostPerDamage peter.Name
display mikey "mikey"