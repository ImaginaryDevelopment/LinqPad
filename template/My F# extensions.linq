<Query Kind="FSharpProgram" />

type String with 
	member x.Before(delimiter:string) : string = 
		x.Substring(0,x.IndexOf(delimiter))
	member x.After(delimiter:string) : string =
		x.Substring(x.IndexOf(delimiter)+delimiter.Length)
	member x.BeforeOrSelf(delimiter:string) : string =
		match x with 
		| x when x.Contains(delimiter) -> x.Before(delimiter)
		| _ -> x
	member x.AfterOrSelf(delimiter:string) : string =
		match x with
		| x when x.Contains(delimiter) -> x.After(delimiter)
		| _ -> x
		
"testing".Before( "ing") |> Dump

"testing".After("test") |> Dump

"testing".BeforeOrSelf("ING") |> Dump

"testing".BeforeOrSelf("ng") |> Dump