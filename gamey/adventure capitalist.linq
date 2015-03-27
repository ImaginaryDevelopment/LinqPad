<Query Kind="FSharpProgram" />

[<Measure>] type m
[<Measure>] type b
[<Measure>] type t
[<Measure>] type q

type BoundedValue = 
	| Small of decimal
	| Million of decimal<m>
	| Billion of decimal<b>
	| Trillion of decimal<t>
	| Quadrillion of decimal<q>
let commas (x:decimal<_>) = 
	(decimal x).ToString("C")
let formatBounded x = 
	match x with
	|Small x -> sprintf "%s" <| commas x
	|Million x -> sprintf "%s %s" <| commas x <| "Million"
	|Billion x -> sprintf "%s %s" <| commas x <| "Billion"
	|Trillion x -> sprintf "%s %s" <| commas x <| "Trillion"
	|Quadrillion x -> sprintf "%s %s" <| commas x <| "Quadrillion"
type Investment = 
	{ Name:string; Time:TimeSpan; Amount: int; Profit: BoundedValue}
	with member x.Display = formatBounded x.Profit
let inline guard x f z = 
	let result = f x
	if result <> z then failwithf "expected f %A to be %A, but was %A" x <| commas z <| commas result
let k x = x * 1000M
let m x = k x * 1000M
let b x = m x * 1000M
let t x = b x * 1000M

let thousand x = BoundedValue.Small <| x * 1000M
let million x = BoundedValue.Million <| x * 1M<m>
let billion x = BoundedValue.Billion <| x * 1M<b>
let quadrillion x = BoundedValue.Quadrillion <| x * 1M<q>



guard 1M k 1000M
guard 1M m 1000000M
guard 1M b 1000000000M
guard 1M t 1000000000000M

let investments = [ 
	{Name="Lemonade Stand"; Time= TimeSpan(0,0,1); Amount= 400; Profit = 10.8M |> thousand}
	{Name="Newspaper Stand"; Time = TimeSpan(0,0,1); Amount = 100; Profit = 54M |> thousand }
	{Name ="Carwash"; Time = TimeSpan(0,0,1); Amount = 100; Profit = 1.458M |> million }
	{Name = "PizzaPlace"; Time = TimeSpan(0,0,1); Amount = 100; Profit = 3.888M |> million}
	{Name = "Doughnut Shop"; Time = TimeSpan(0,0,1); Amount = 100; Profit = 46.656M |> million}
	{Name = "Shrimp Boat"; Time = TimeSpan(0,0,3); Amount = 100; Profit = 559.872M |> million}
	{Name = "Hockey Team"; Time = TimeSpan(0,0,12); Amount = 100; Profit = 6.718M |> billion}
]

investments.Dump()