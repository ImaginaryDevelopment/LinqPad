<Query Kind="FSharpProgram" />


[<Measure>] type lb 
[<Measure>] type oz
[<Measure>] type kg 
[<Measure>] type inch 
[<Measure>] type cm 
[<Measure>] type m 
[<Measure>] type bmi = kg / m^2

type Height = 
	| ImperialInch of float32<inch>
	| MetricM of float32<m>
	| MetricCm of float32<cm>
	
type Weight =
	| ImperialLb of float32<lb>
	| ImperialOz of float32<oz>
	| MetricKg of float32<kg>

let mFromCm x = x * 0.01f<m/cm>
let cmFromInches x = x * 2.54f<cm/inch>  //let cm =  inches * 2.54
let lbFromOz x = x / 16.0f<oz/lb>

let mFromHeight x =
	match x with
	| Height.ImperialInch inch -> cmFromInches inch |> mFromCm
	| Height.MetricM m -> m
	| Height.MetricCm cm -> mFromCm cm
	
let kgFromLbs x = x * 0.453592f<kg/lb> // let kg = lbs * 0.453592

let kgFromWeight x = 
	match x with
	|Weight.ImperialLb lb -> kgFromLbs lb
	|Weight.ImperialOz oz -> lbFromOz oz |> kgFromLbs
	|Weight.MetricKg kg -> kg

let m2FromM (x:float32<m>) = (float32 x) ** 2.f * 1.f<m^2>

let maybeRead title (unit:float32<_>) f = 
	let raw = Util.ReadLine<string>(title)
	if String.IsNullOrEmpty raw then None else
	let success,value = Double.TryParse raw
	if success then unit * float32 value|> f |> Some else None

let inline addOpt f opt start= 
	Option.fold (fun state value -> f value + state) start opt

let totalHeight:float32<m>= 
	let addHeight = addOpt mFromHeight
	let inches =  maybeRead "(inches,cm,meters) inches?" 1.0f<inch> Height.ImperialInch
	let cm = maybeRead "(inches,cm,meters) cm?" 1.0f<cm> Height.MetricCm
	let meters = maybeRead "(inches,cm,meters) meters?" 1.0f<m> Height.MetricM
	
	let result = 
		addHeight inches 0.0f<m> 
		|> addHeight meters
		|> addHeight cm
	printfn "%A %A %A = %A m" inches cm meters result
	result

let totalWeight= 
	let addWeight = addOpt kgFromWeight
	let lbs = maybeRead "(lbs,oz,kg) lbs?" 1.0f<lb> Weight.ImperialLb
	let oz = maybeRead "(lbs,oz,kg) ounces?" 1.0f<oz> Weight.ImperialOz
	let kg = maybeRead "(lbs,oz,kg) kg?" 1.0f<kg> Weight.MetricKg
	let result = 
		addWeight lbs 0.0f<kg>
		|> addWeight oz
		|> addWeight kg
	printfn "%A %A %A = %A kg" lbs oz kg result
	result
	
let calcBmi weight height =
	let h = mFromHeight height
	let w = kgFromWeight weight
	let divisor = m2FromM  h
	w / divisor
let trunc (x:float32<_>) =
	float <| x * 10.f 
	|> Math.Truncate 
	|> float32 
	|> fun e -> e / 10.f // float32 (Math.Truncate(float <| x * 10.f)) / 10.0f 
type Result = { Rounded:float32; Trunc:float32}
let round x = Math.Round(float x,1) |> float32
calcBmi <| MetricKg totalWeight <| MetricM totalHeight 
|> fun e -> eprintfn "%f kg/m^2" (e / 1.0f<bmi>); e
|> fun e -> e.Dump("bmi"); e
|> fun e -> {Rounded=round e;Trunc = trunc e}
|> fun e-> e.Dump("bmi govt")
//{inches,lbs,cm,kg,bmi}.Dump();