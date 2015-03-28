<Query Kind="FSharpProgram" />

type RelicInfo = { Row:int; Stage:int; RelicBonus:float; RelicsWithAHM:float}
let relicChart start=
	let next current = 
		current+15
	let formulaBasedRecalc x = Math.Floor(Math.Max(0,90+x*15-75) / 15 |> fun y -> Math.Pow(float(y),1.7))
	let nextValue i = (Seq.fold (fun state _ -> next state) start [1..i])
	[0..33]
	//|> Seq.map (fun i -> i,(Seq.fold (fun state _ -> next state) start [1..i]))
	|> Seq.map (fun i -> i + 2,nextValue i, formulaBasedRecalc i)
	|> Seq.map ( fun (r,stage,recalc) -> {Row=r;Stage=stage;RelicBonus= recalc;RelicsWithAHM= recalc * 2.})
	//|> Seq.fold (fun state index -> next state) start
	|> Dump
relicChart 90

let heroCostNext (cost:float<_>)  = cost * 1.075
let heroCostNextX x cost = [ 0.. x-1] |> Seq.fold (fun state _ -> heroCostNext state) cost
let heroCostTotalxy x y cost = [x..y-1] |> Seq.fold(fun state _ -> heroCostNext state + state) cost
let heroCostTotal x cost = heroCostTotalxy 1 x cost
let heroBonuses = [(1,10); (11,25);(26,50);(51,100);(101,200);(201,400);(401,800)]
let takedaChart start stop (startingCost: float option) =
	let mutable nextLevelCost = if startingCost.IsSome then startingCost.Value else 50.
	let nextValue () = heroCostNext nextLevelCost
	[start..stop]
	|> Seq.map(fun i -> 
	let currentCost = nextLevelCost
	nextLevelCost <- nextValue()
	i,currentCost
	)
	|> Dump
takedaChart 1 10 None
let takedaCostPerBonus (* level *) (startingCost: float option) = 
		
	let start = if startingCost.IsSome then startingCost.Value else 50.
	let mutable nextCost = start
	heroBonuses
	|> Seq.mapi ( fun  i (x,y) -> 
		let totalCost = heroCostTotalxy x y nextCost
		nextCost <- heroCostNextX (y-x) nextCost 
		(sprintf "Bonus %i" i,totalCost )
			)
	|> Array.ofSeq
takedaCostPerBonus None
|> Dump