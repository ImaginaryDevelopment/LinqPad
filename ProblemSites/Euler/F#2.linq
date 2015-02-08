<Query Kind="FSharpProgram" />

open System
//module SysExtensions =
//	type System.Int32 with
//		member this.Thousand() x = x * 1000
//	type System.Int16 with
//		member this.Thousand() x= x * 1000
type System.Int32 with
	//member this.Thousand() = this * 1000
	member this.Thousand
		with get() = this * 1000
	member this.Million
		with get() = this.Thousand * 1000
//open SysExtensions
//(5).Million |> Dump


let rec fib a b = seq { yield a; yield! fib b (a+b) }

let sample = Seq.take 10 (fib 1 2) 
sample |> Dump 
sample |> Seq.fold (fun acc elem -> acc + elem) 0 |> Dump

let evens = fib 1 2 |> Seq.filter (fun e-> e % 2 =0)

let under4Mil =  Seq.takeWhile (fun e -> e<(4).Million) evens

under4Mil |> Seq.reduce (fun a b -> a+b) |> Dump



