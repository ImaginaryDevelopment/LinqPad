<Query Kind="FSharpProgram" />

// Tap Titans Maths

//http://www.reddit.com/r/TapTitans/comments/300cmm/question_about_gold_increase_artifacts/
// Monster Gold = stage gold
// Boss Gold = stage gold * 6
// Chest Gold = stage gold * 10
// normal mob = monstergold * (heroes bonus gold + future's fortune) * amulet of the valrunes * crafter's elixir * 10 if divine chalice triggers
// boss = boss gold * (heroes bonus gold + future's fortune) * knight's shield * crafter's elixir
// chesterson = chest gold * (heroes bonus gold + future's fortune) * crafter's elixir * chest of contentment
type GoldSource = 
	| Monster
	| Boss
	| Chest
	| Chesterson
let goldForX source stageGold heroesBonus futuresFortune amuletOfValrunes craftersElixir (divineChaliceChance:float option) knightsShield chestOfContentment = 
	let divineChalice = if divineChaliceChance.IsSome then 10*divineChaliceChange.Value else 1
	match source with
	| Monster -> stageGold * (heroesBonus + futuresFortune) * amultOfValRunes * craftersElixir * divineChalice
	| Boss -> stageGold * 6 * (heroesBonus + futuresFortune) * knightsShield * craftersElixir
	| Chesterson -> stageGold * 10 *(heroesBonus + futuresFortune) * craftersElixir * chestOfContentment
	| Chest -> stageGold * 10
// artifact multiplier: 6673%
// level 1 dmg 83
// level 2 dmg +92 -5 = 176 damage
// level 3 dmg +101 -6 = 277 damage
// level 4 dmg +111 -8 = 388 damage
// level 5 dmg +132 -10 = 509 damage

// Takeda 1 = 391 dps -50
// Takeda 2 = +398 dps -54 = 790
// Takeda 3 

// Contessa 1 weapon 	= 1.94k 	- 175
// 			2 			= 1.97k 	- 189

// Hornetta 			= 16.60k 	- 2.85k 

// Stage 2 hp 45
// Stage 3 hp 71
// Stage 7 hp 434
// Stage 12 hp 4.14k
[<Measure>] type K
[<Measure>] type M
[<Measure>] type b
[<Measure>] type T
[<Measure>] type aa
[<Measure>] type bb
[<Measure>] type cc
[<Measure>] type dd
[<Measure>] type ee
[<Measure>] type ff
[<Measure>] type gg
[<Measure>] type hh
[<Measure>] type ii
[<Measure>] type jj
[<Measure>] type kk
// let measurements = [ 1.0<K>; 1.0<M>; 1.<b>;1.<T>;1.<aa>;1.<bb>;1.<cc>;1.<dd>;1.<ee>;1.<ff>;1.<gg>]
let ffToGg (f:float<ff>) = f / 1000.<ff> * 1.0<gg>
type TitanNumber =
	| K of float<K>
	| M of float<M>
	| B of float<b>
	| T of float<T>
	| AA of float<aa>
	| BB of float<bb>
	| CC of float<cc>
	| DD of float<dd>
	| EE of float<ee>
	| FF of float<ff>
	| GG of float<gg>
	| HH of float<hh>
	| II of float<ii>
	
let measurements = [ 
		TitanNumber.K 1.0<K>
		TitanNumber.M 1.0<M>
		TitanNumber.B 1.<b>
		TitanNumber.T 1.<T>
		TitanNumber.AA 1.<aa>
		TitanNumber.BB 1.<bb>
		TitanNumber.CC 1.<cc>
		TitanNumber.DD 1.<dd>
		TitanNumber.EE 1.<ee>
		TitanNumber.FF 1.<ff>
		TitanNumber.GG 1.<gg>
	]
measurements.[0] |> Dump
let dmg = System.Numerics.BigInteger(1)

let dumpT (title:string) a = a.Dump(title)

let artMod = 6557
type Currency = 
	| Coins
	| Relics
	| TournamentPoints
type Modification =
	| TapDamage
	| HeroDamage
	| Dps
	| AllDamage
type Artifact = Modification*Modification

let heroCostNext (cost:float<_>)  = cost * 1.075
let heroCostNextX x cost = [ 0.. x-1] |> Seq.fold (fun state _ -> heroCostNext state) cost
let heroCostTotal x cost = [1.. x-1] |> Seq.fold(fun state _ -> heroCostNext state + state) cost

(heroCostNext 47.60,heroCostNextX 1 47.60)
|> dumpT "heroCostNext, heroCostNextX where x is 1"

(47.60 + 51.17,heroCostTotal 2 47.60)
|> fun e -> e.Dump("up two levels, heroCostTotal")

heroCostNextX 25 47.60 |> Dump

heroCostNextX 100 47.60 |> fun e -> (/) e 47.60 |> Dump
let heroTo756 = 6.39<dd>
let heroTo757 = heroCostNext heroTo756
(heroTo756,heroTo757)
|> dumpT "to 756, to 757,in dd, ui says 6.87dd"
let hamletteTo52 = 855.55<ff>
let hamletteTo62 = 
	hamletteTo52|> heroCostTotal 10
	|> ffToGg
	|> dumpT "total cost in gg"