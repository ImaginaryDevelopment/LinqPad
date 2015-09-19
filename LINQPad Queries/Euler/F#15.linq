<Query Kind="FSharpProgram" />

#light


let size = 20L
let start = (0L,0L)
let GoRight (x,y) = (x+1L,y)
let GoDown (x,y) = (x,y+1L)
let divide = fun a b -> b / a
//start |> GoRight |> Dump

let rec GetPaths (x,y) = 
	let (dx,dy) = (x,y+1L)
	let (rx,ry) = (x+1L,y)
	let d = dx <=size && dy <=size
	let r = rx<=size && ry <=size
	if (d && r) then
		2L+ GetPaths(dx,dy) + GetPaths(rx,ry)
	else
	if (d) then
		1L + GetPaths(dx,dy)
	else
	if (r) then
		1L + GetPaths(rx,ry)
	else
	0L
	
	
GetPaths start |> divide 3L  |> Dump