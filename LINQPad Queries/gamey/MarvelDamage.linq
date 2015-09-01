<Query Kind="FSharpProgram" />

[<Measure>] type Physical
[<Measure>] type Melee

let abs (x:float<_>) = Math.Abs(x)

let dmg = 2055 // without the items you are about to add for comparison
let physical = 2944<Physical> // without the items you are about to add for comparison
let melee = 1957<Melee> // without the items you are about to add for comparison

let powerDamageAverage x y = (x+y) /2

let basePowerDamage = powerDamageAverage 23582 35373 |> float // the power damage without the items you are going to wear

let excal = 1000<Melee>
let runes = 261<Melee>
let crimson = 515<Physical>
let uru = 100<Physical>

let maxMelee = melee + excal + runes
let maxPhysical = physical + crimson + uru

let coeff (adjustedAvg:float) (increase:float<_>) = (adjustedAvg - basePowerDamage ) / increase // increase is how much the dmg type went up with the items equipped

let err exLow exHigh actual = exLow+exHigh |> (/) 2.<_> |> (-) actual |> abs

let meleeCoeff increase = 
	powerDamageAverage 37271 24848
	|> float
	|> fun e -> coeff e
	|> fun ce -> ce <| increase
	
type Analysis = { Increase:int; ErrAmount: double;ErrPercent:double; Actual: double; Expected:double}	

let myMeleeCoeff = 
	let increase = float excal + float runes
	meleeCoeff increase
let phyCoeff =
	let tooltip = powerDamageAverage 39625 26177
	let increase = crimson + uru
	let co = 
		tooltip
		|> float
		|> fun e -> coeff e
		|> fun ce -> ce <| float increase
	let calculated = (co / 1.<Physical> * float increase) + basePowerDamage
	{Increase = increase; ErrAmount = actual - basePowerDamage |> abs; ErrPercent= calculated / tooltip; Actual = calculated; Expected = tooltip}
	

phyCoeff.Dump("physical")
myMeleeCoeff.Dump("melee")

(*
public enum DamageSourceType {
	Physical,
	Energy,
	Mental
}

double EffectiveDamageRating (double damageRating, DamageSourceType type,byte str, byte fighting, byte energy){
	
	double adjustedSourceValue=0.0;
	switch(type){
	case DamageSourceType.Physical:
		adjustedSourceValue = str*160;
		break;
		case DamageSourceType.Energy:
		case DamageSourceType.Mental:
		adjustedSourceValue = energy * 160;
		break;
	}
	return adjustedSourceValue + fighting *120 + damageRating;
} *)