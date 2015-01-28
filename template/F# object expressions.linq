<Query Kind="FSharpProgram" />

type AnyCell = 
  abstract Value : int with get, set
  abstract Print : unit -> unit
// Object expressions - http://tomasp.net/blog/fsharp-iii-oop.aspx/
let newCell n =
    let data = ref n
    { new AnyCell with
      member x.Print() = printf "Data: %d" (!data)
      member x.Value 
        with get() = !data
        and set(v) = data:=v }
		
newCell.Dump()
// val newCell : int -> AnyCell