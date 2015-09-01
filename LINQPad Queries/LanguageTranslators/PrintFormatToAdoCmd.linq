<Query Kind="FSharpProgram" />

//let cmdString = "update claims set SecondaryGuarantorProfileInfoID = {0},maxlevel = {1} where  appointmentid = {2}"
open System
type String with
    static member replace (target:string) (replacement) (str:string) = str.Replace(target,replacement)

let buildCmdString fs arg i :string*string*obj = 
    let applied = sprintf fs arg
    let replacement = (sprintf"{%i}" i)
    let replace target str = String.replace target replacement str
    let cmdPart = 
        fs.Value
        |> replace "'%s'" 
        |> replace "'%i'" 
        |> replace "'%d'"
    applied,cmdPart, upcast arg

let buildUpdateStatement secondaryGuarantorProfileInfoId maxLevel appointmentId = 
    let items = 
		[ 
			buildCmdString "update claims set SecondaryGuarantorProfileInfoID = '%i'," secondaryGuarantorProfileInfoId
			buildCmdString "maxlevel = '%s' " maxLevel
			buildCmdString "where appointmentid = '%i'" appointmentId
    	]
    	|> Seq.mapi(fun i f -> f i)
	let statement = items |> Seq.fold (fun state (a,r,arg) -> state + a) String.Empty
	let cmdPart = items |> Seq.fold(fun state (a,r,arg) -> state + r) String.Empty
	let args = items |> Seq.map (fun (_,_,arg) -> arg)
	statement,cmdPart,args
let statement,cmdPart,args = buildUpdateStatement 52 "21" 35


printfn "%s" statement
printfn "%s" cmdPart
args.Dump()
// not working aggregate:
//let buildCmdStringFromSeq (fsArgPairs: ((_ ->string)*_) seq) = 
//    fsArgPairs 
//    |> Seq.fold( fun state (fs,arg:_) -> state + (fs arg)) String.Empty