<Query Kind="FSharpProgram" />

// stash changes to target repo (target files in target repo?)
// strip specified lines and blacklist items 
// or replace matches with redacted.

let targetDir = @"C:\projects\config"
let fullPass = false
let trim (s:string) = s.Trim()

let dump a =
	a |> Dump
	a
	
let dumpf (t:string) f a = 
	(f a).Dump(t)
	a
let trimc chars (s:string) = s.Trim(chars)
let gitSave appliedAction = 
	if not <| System.IO.Directory.Exists targetDir then
		failwith "target doesn't exist"
	
	Environment.CurrentDirectory <- targetDir
	//Util.Cmd("git","--help stash").Dump();
	Util.Cmd("git","stash save tempstash"+DateTime.UtcNow.ToString("yyyyMMdd.hh.ss")).Dump() // --keep-index  ?
	
	Util.Cmd("git","stash apply").Dump()
	// make modifications
	appliedAction() // probably a commit on the command line, or gui
	
	// warning reset hard if for some reason stash failed, is super dangerous, I'm super cereal
	Util.Cmd("git","reset --hard").Dump()
	Util.Cmd("git","stash pop").Dump("pop")

let uncommitted = 
	Util.Cmd("git","status -s --untracked-files=no",true)
	|> Seq.map trim
	|> Seq.map (fun l-> 
		//l.[0].ToString(), 
		String(l.Skip(1).ToArray()).Replace("/","\\") |> trim |> trimc [|'\"'|]
		)
	//|> Seq.map (fun (modType, relativePath) -> modType,relativePath, lazy(System.IO.Path.Combine(targetDir,relativePath)))
let wordBlacklists= dict ["foopassword", (fun (_:string)-> "[redacted]")]

let modifyFiles (files:string seq) = 
	let modifyFile (f:string) = 
		let lineBlacklists = ["<foobadTag>"]
		
		let redact (line:string) :string*int= 
			wordBlacklists.Keys
			|> Seq.map(fun k-> k,0)
			|> Seq.fold ( fun acc elem -> 
				let line,redactCount = acc
				let key,_ = elem
				if line.Contains(key) then 
					line.Replace(key, wordBlacklists.[key] line |> dump),redactCount + 1
				else line,redactCount
			) (line,0)
			
		(* for lines that should be blacklisted if the word blacklister did not redact.
		//let redactableLineBlackLists = ["osx"] *)
		let text = System.IO.File.ReadAllText f
		let lines = text.SplitLines() |> dumpf "length" Seq.length
		let length = Seq.length lines
		let redacted = 
			lines 
			|> Seq.filter ( fun e->not <| lineBlacklists.All( fun lb -> lb.Equals(e)))
			|> Seq.map redact
			//TODO: put the lines back together while summing the redaction counts, to roll them up by file
			|> fun e->String.Join(Environment.NewLine,e) //Seq.fold (fun acc elem -> acc+elem) String.Empty
			
		()
	files
	|> Seq.iter modifyFile
uncommitted 
|> Seq.take(2)
|> modifyFiles
 