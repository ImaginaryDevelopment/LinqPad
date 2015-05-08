<Query Kind="FSharpProgram" />

open System
open System.Linq

let debug=false

let compareBytes = Seq.compareWith (fun a b ->
	if a > b then 1
	elif a < b then -1
	else 0)	
	
let runLengthEncode (a:byte[])   =
	a //needs more code
	
let test a b :bool =
	try
		let actual=runLengthEncode a |> List.ofArray
		let seq=compareBytes  actual b
		if debug then
			Console.Write "\tresult:"
			Console.WriteLine seq
		seq = 0
	with
		| ex -> 
		
			Console.WriteLine("fail!" + ex.Message)
			false
			
let output a expected c =
	let prefix = (fun i ->
		Console.Write "Test "
		Console.Write (i.ToString())
		Console.Write " "
		)
	if test (Seq.toArray(a)) expected then prefix( c); Console.WriteLine "Pass" else
	prefix(c)
	Console.WriteLine "Fail"
	//linq pad side by side
	//Dump (Util.HorizontalRun(false, "actual","expected"))
	//Dump (Util.HorizontalRun(false, a,expected))
	Console.Write "Actual:"
	Console.WriteLine a
	Console.Write "Expected:"
	Console.WriteLine expected

let test1 = [ 1uy;1uy;1uy;2uy;2uy;2uy];
let test1Expected= [3uy;1uy;3uy;2uy];

output test1 test1 0
output test1 test1Expected 1

let test2 = [ 0uy; 1uy; 2uy; 3uy]
let test2Expected = [  1uy; 0uy; 1uy; 1uy; 1uy; 2uy; 1uy; 3uy]
output test2 test2Expected 2

let test3 = seq { for n in 0..299 do if n<270 then yield 12uy else yield 3uy}
//Dump test3
test3 |> Seq.countBy (fun e -> if e=3uy then 1 else 0) |> Console.WriteLine
