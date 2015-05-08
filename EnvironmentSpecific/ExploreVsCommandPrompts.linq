<Query Kind="FSharpProgram" />

//simple fix was change shortcut from pointing at 11.0 to 12.0
type String with 
	member this.Before(delimiter:string) : string = 
		this.Substring(0,this.IndexOf(delimiter))
	member x.After(delimiter:string) : string =
		x.Substring(x.IndexOf(delimiter) + delimiter.Length)
		
let getEnvVariable s = Environment.ExpandEnvironmentVariables(s)
let matches pattern text = Regex.Matches(text,pattern) |> Seq.cast<Match> |> Seq.map (fun m-> m.Value)
let getFilesRec start pattern = Directory.EnumerateFiles(start,pattern, SearchOption.AllDirectories)
let after (s:string) delimiter = s.After(delimiter)

//for c in commonKeys do
//	printfn "%s - %A" c dic.[c]
let checkBatFile b = 
	let text = File.ReadAllText b
	let envRefs = 
		matches "%[^\r\n% ]+%" text
		|> Seq.distinct
		|> Array.ofSeq
	let envSets = matches "set \"(\w+)=.*\"" text
	
	if Seq.isEmpty envRefs then text.Dump(b) 
		else
		envRefs
		|> Seq.map (fun er -> er, 
								let location = getEnvVariable er
								if location = er then "bad" else if Directory.Exists er then String.Empty else "missing"
			)
		|> fun vars -> (vars,text)
		|> fun x -> x.Dump(b)
		//( envRefs,envSets,text).Dump(b)
		
		
let bats = 
	let vsFolders = 
		Directory.EnumerateDirectories( Environment.GetFolderPath Environment.SpecialFolder.ProgramFilesX86, "Microsoft Visual Studio*")
		|> Seq.sortBy ( fun d -> 
			Decimal.Parse <| after d "Visual Studio ")
		|> Seq.map (fun d -> Path.Combine(d,"VC"))
	seq {
		for folder in vsFolders do
			//folder.Dump()
			yield! getFilesRec folder "*vars*.bat"
		}
	|> Seq.iter checkBatFile
bats.Dump()

let dic = Environment.GetEnvironmentVariables()
let commonKeys = 
	dic.Keys
	|> Seq.cast<string>
	|> Seq.filter (fun k -> k.Contains("COMNTOOLS"))
