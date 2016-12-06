<Query Kind="FSharpProgram" />

// recommendations from http://pastebin.com/t8uBRVLk

// past 99999/1/770 (100% autobuy)
type IdealAmounts = {MaxClones:int; CreationCount:int; BuildingSpeed:int}


let start = {MaxClones=99999; CreationCount = 1; BuildingSpeed=770}

Seq.initInfinite(fun x -> {start with MaxClones = start.MaxClones + 20000 * x; CreationCount = start.CreationCount + 2 * x; BuildingSpeed = start.BuildingSpeed + 175*x})
|> Seq.take 5
|> Dump
|> ignore