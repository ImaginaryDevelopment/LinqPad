<Query Kind="FSharpProgram" />

type EnumerateResult = | Success of string seq | Failure of Exception * string //| TooLong of PathTooLongException * string | Unautho
let flatten (seqOfSequences: _ seq seq) = Seq.collect id seqOfSequences (* seq { for x in i do yield x} *) (* id is a magical keyword? *)

let tryEnumerateDirectoryFiles root filepattern = 
	try
		 Directory.EnumerateFiles(root, filepattern) |> Success
	with 
	| :? UnauthorizedAccessException
	| :? PathTooLongException  as ex ->
		ex.Data.["path"] <- root; Failure (ex,root)
//	| :? PathTooLongException as ex -> 
//		ex.Data.["path"] <- root; Failure ex
	
let rec findAllFiles root filepattern: EnumerateResult seq = 
	seq {
		yield Success <| Directory.EnumerateFiles(root,filepattern)
		for d in Directory.EnumerateDirectories(root) do
			match tryEnumerateDirectoryFiles d filepattern with
			| Success files -> 
				yield Success files
				yield! findAllFiles d filepattern
			| Failure (ex,dir) -> yield Failure (ex,dir)
	}
let results = findAllFiles "C:\\" "tf.exe" |> Array.ofSeq

results
//|> (fun r -> printfn "%A" r;r)
|> Seq.choose (fun r -> match r with | Success x -> Some x | Failure _ -> None)
//|> (fun r -> printfn "%A" r;r)
|> flatten // Seq.collect (fun i -> seq { for f in i do yield f})
|> Dump

//Util.Cmd("tf").Dump();
type ExceptionDetails = { Message:string; Path:string; InnerException:Exception; StackTrace:string; Data:IDictionary<obj,obj>;ExceptionType:string}
results
|> Seq.choose (fun r -> match r with | Failure (ex,dir) -> Some (ex,dir) |Success _ -> None)
//|> Seq.map( fun (ex,dir) -> ex.Message,dir, ex.InnerException,ex.StackTrace,ex.Data,ex.GetType().FullName)
//|> Seq.collect (fun i -> seq { for f in i do yield f})
|> Dump //(fun r -> printfn "collected %A" r;r)
//|> ignore