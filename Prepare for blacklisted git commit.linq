<Query Kind="FSharpProgram" />

// stash changes to target repo (target files in target repo?)
// strip specified lines and blacklist items 
// or replace matches with redacted.

let targetDir = @"C:\projects\config"
let fullPass = false
let trim (s:string) = s.Trim()
let cmd (s:string) (args:string) = Util.Cmd(s,args)
let cmdq (s:string) (args:string) (quiet:bool) = Util.Cmd(s,args,quiet)
let dump a =
	a |> Dump
	a
let dumps t a=
    printfn "%s" t
    printfn "%A" a
	
let dumpf (t:string) f a = 
	(f a) |> dumps t
	a
let trimc chars (s:string) = s.Trim(chars)
let gitSave appliedAction= 
	if not <| System.IO.Directory.Exists targetDir then
		failwith "target doesn't exist"
	
	Environment.CurrentDirectory <- targetDir
	let git = cmd "git"
	//Util.Cmd("git","--help stash").Dump();
	git  ("stash save tempstash"+DateTime.UtcNow.ToString("yyyyMMdd.hh.ss")) |> Dump // --keep-index  ?
	
	git  "stash apply" |> Dump
	// make modifications
	appliedAction() // probably a commit on the command line, or gui
	
	// warning reset hard if for some reason stash failed, is super dangerous, I'm super cereal
	git "reset --hard" |> Dump
	git "stash pop" |> dumps "pop"

let uncommitted () = 
	cmdq "git" "status -s --untracked-files=no" true
	|> Seq.map trim
	|> Seq.map (fun l-> 
		//l.[0].ToString(), 
		String(l.Skip(1).ToArray()).Replace("/","\\") |> trim |> trimc [|'\"'|]
		)
	//|> Seq.map (fun (modType, relativePath) -> modType,relativePath, lazy(System.IO.Path.Combine(targetDir,relativePath)))
	
let wordBlacklists:IDictionary<string,string->string> = dict ["oceanside", fun _ -> "[redacted]"]
let regexTextMap:IDictionary<Regex,string->string option> = dict[Regex("<Connection>\s+<ID>\s+</Connection>",RegexOptions.Multiline), fun _ -> None]
let fileBlacklists:IDictionary<string,string->string option> = dict ["Prepare for blacklisted git commit.linq", fun  _-> None]
let itemsWithCount (a:('a*int) list ) : 'a list * int = 
    List.foldBack (fun (str, i) (strs, sum) -> str :: strs, i + sum) a ([], 0)

let redact (line:string) :string*int= // instead, consider using Some Vs. none to tell if the line had modifications
            wordBlacklists.Keys
            |> Seq.map(fun k-> k,0)
            |> Seq.fold ( fun acc elem -> 
                let line,redactCount = acc
                let key,_ = elem
                if line.Contains(key) then 
                    line.Replace(key, wordBlacklists.[key] line),redactCount + 1
                else line,redactCount
            ) (line,0)
let modifyFile (filename:string) :string*int = 
        let lineBlacklists = ["<Server>"]

        (* for lines that should be blacklisted if the word blacklister did not redact.
        //let redactableLineBlackLists = ["osx"] *)
        let text = System.IO.File.ReadAllText filename
        let lines = text.SplitLines() // |> dumpf "length" Seq.length
        let length = Seq.length lines
        let redacted : string*int = 
            lines 
            |> Seq.filter ( fun e->not <| lineBlacklists.All( fun lb -> lb.Equals(e)))
            |> Seq.map redact
            |> List.ofSeq
            //|> dump
            //TODO: put the lines back together while summing the redaction counts, to roll them up by file
            //|> fun line count -> String.Join(Environment.NewLine,line),count //Seq.fold (fun acc elem -> acc+elem) String.Empty
            |> itemsWithCount
            |> (fun (lines,count) -> String.Join(Environment.NewLine,lines),count)
        redacted
let modificationMap (f:string->string*int) (filename:string) : string*string*int =
	let mc = f filename
	let modified,count = mc // expand
	filename, modified,count
	
let modifyFiles (modification:string->string*int) (files:string seq)  : (string*string*int) seq = 
	let modifyMap (s:string) : string*string*int = modificationMap modification s
    files
    |> Seq.map modifyMap //(fun (filename:string) -> modifyMap filename)
	
printfn "executing from %s" Environment.CurrentDirectory
uncommitted()
|> dump
|> Seq.filter (fun filename -> fileBlacklists.Keys.Any(fun fb-> filename.Contains(fb))=false)
|> dump
|> modifyFiles modifyFile
|> Seq.filter(fun (_,_,c) -> c > 0)
|> Dump