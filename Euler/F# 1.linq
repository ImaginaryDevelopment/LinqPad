<Query Kind="FSharpProgram" />

open System

let naturals = seq { for i in 1..999 do if (i % 5 =0 || i % 3 = 0) then yield i}
let sum= Seq.sum naturals
Dump sum