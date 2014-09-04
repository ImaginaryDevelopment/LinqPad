<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Namespace>System.IO</Namespace>
</Query>

open System
//open System.Diagnostics
open System.IO
//open System.Windows.Forms

let doTestFileExclude = false
let highestLinesByFileMinimum = 550
let highestLinesByFolderMinimum = 2000
let highestMagicByFileMinimum = 6

let useThresholds = true
let useTakeLimits = Some(20)

//let after (value:string) (delimiter:string) = 
let endsWithIgnore (value:string ,test:string) = value.EndsWith(test,StringComparison.CurrentCultureIgnoreCase)
let startsWithIgnore (value:string ,test:string) = value.StartsWith(test,StringComparison.CurrentCultureIgnoreCase)
let fileExcludeEndings = ["designer.cs";"generated.cs";"codegen.cs"]

let fileExclude	(a:string):bool = 
	(a, "designer.cs") |> endsWithIgnore ||
	(a,"jquery-") |> startsWithIgnore ||
	(a, "AssemblyInfo") |> startsWithIgnore
	
let pathExcludeEndings = ["obj"; "Debug";".sonar";"ServerObjects";"Service References";"Web References";"PackageTmp";"TestResults";"packages";"$tf";".git";"bin" ]

let pathExclude (a:string) :bool =
	List.exists ( fun elem -> (a,elem)|> endsWithIgnore) pathExcludeEndings ||
	a.Contains(@"NonSln") ||
	a.Contains(".localhost") ||
	a.Contains("Generated_C#_Source")

//record, class, struct, or discriminated union?	
type  CountSettings = {
	Path: string
	Patterns: IEnumerable<string>
	FileExclude: string -> bool
	PathExclude: string-> bool
	}
	
//instance of above type
let currentSettings:CountSettings=	{
	Path=
		let userPath=Util.ReadLine("SourceDirectory?",@"%devroot%")
		let userExpanded= if userPath.Contains('%') then System.Environment.ExpandEnvironmentVariables(userPath) else userPath
		let exists=System.IO.Directory.Exists(userExpanded)
		if not(exists) then //guard clause
			raise(DirectoryNotFoundException(userExpanded))
		do userExpanded.Dump("Searching")
		userExpanded
	Patterns=["*.cs";"*.aspx";"*.ascx"]
	FileExclude=fileExclude
	PathExclude=pathExclude
	}
	
//set cwd (not a functional call, imperative?)
System.Environment.CurrentDirectory <- currentSettings.Path

let rec getDirectories (basePath:string) dirFilter= seq{
	for d in Directory.EnumerateDirectories(basePath) do
		if not(dirFilter d) then
			yield d
			yield! getDirectories d dirFilter
	}

let includedDirectories=getDirectories currentSettings.Path currentSettings.PathExclude

let getFilesByPatterns directories patterns =
	seq{
		for d in directories do
		for pattern in patterns do
			for file:string in Directory.EnumerateFiles(d,pattern) do
				yield file
	}

let allFiles = getFilesByPatterns includedDirectories currentSettings.Patterns

//rec means recursive function
let filterFiles files fileFilter= seq{
	for file in files do
		let filename=System.IO.Path.GetFileName(file)
		if not(fileFilter(filename)) then
			yield file
	}
	
let filterFilesResult= filterFiles allFiles currentSettings.FileExclude |> Seq.toArray

type MetaData = {SearchDir:string;DirectoriesIncluded:int; TotalFilesMatchingPatternList:int; TotalFilesIncluded:int;TotalLinesOfCode:int}

let mutable metaData = {
	MetaData.SearchDir= currentSettings.Path
	DirectoriesIncluded = includedDirectories |> Seq.length
	TotalFilesMatchingPatternList = allFiles |> Seq.length
	TotalFilesIncluded = filterFilesResult |> Seq.length
	TotalLinesOfCode = 0
	}
	
metaData.Dump("before line counts")

type FileSummary(relativePath:string, fullPath:string,readerFunc:string->string[]) = 
	let rgNumber = new Regex(@"\.?[0-9]+(\.[0-9]+)?", RegexOptions.Compiled)
	let prepend="~"+if relativePath.StartsWith("\\") then "" else "\\" 
	member self.FullPath with get() = fullPath
	member self.Filename with get() = System.IO.Path.GetFileName(self.FullPath)
	member self.RelativePath with get() = prepend+relativePath.Substring(0,relativePath.Length-self.Filename.Length)
	member private self.AllLines=lazy(self.FullPath |> readerFunc)
	member private self.AllText=lazy( self.AllLines.Value |> String.concat "")
	member self.LineCount = self.AllLines.Value |> Seq.length
	member self.Nonspaces=lazy(self.AllText.Value |> Seq.filter (fun x->Char.IsWhiteSpace(x)<>true) |> Seq.length)
	member self.DoubleQuotes=lazy(self.AllText.Value |> Seq.filter (fun x-> '"'=x) |> Seq.length)
	member self.PotentialMagicNumbers=lazy(self.AllText.Value |> rgNumber.Matches |> fun x->x.Count)
	
let  asSummary (files:string[]) :seq<FileSummary> =
	let uriPath (r:string)= if r.Length>1 then "~"+r.Substring(1) else String.Empty //if relPath is .
	let reader x = System.IO.File.ReadAllLines(x)
	seq{
	
		for file:string in files do
			let relPathWithFilename=file.Substring(currentSettings.Path.Length)//.Dump()
			//do file.Dump(relPath)
			let summary=new FileSummary(relPathWithFilename,file,reader)
			yield summary
	}

let summaries = asSummary filterFilesResult
let linesOfCode = summaries |> Seq.map (fun e-> e.LineCount) |> Seq.sum

metaData <- {metaData with TotalLinesOfCode = linesOfCode}
metaData.Dump()

//let makeButton fs=
//	let handler(e) = fs.Dump()
//	let button=new System.Windows.Forms.Button()
//	do button.Text="Open folder"
//	do button.Click.Add handler
//	button
	
let makeLinq (path:string) (t:string) = 
	let dirPath=System.IO.Path.GetDirectoryName(path)
	let wrapper (m:string) = "Process.Start(\"" + m.Replace(@"\",@"\\") + "\")"
	let rawHtml (d:string) = "<a href=\""+d+"\">"+t+"</a>"
	new Hyperlinq(QueryLanguage.Expression,wrapper(dirPath),t)
	
let buildLimitString f title threshold = 
	let label = sprintf "%s%s%s" title (if useThresholds then " > "+ threshold.ToString() else "") (if useTakeLimits.IsSome then " (top "+useTakeLimits.Value.ToString()+")" else "")
	f(threshold).Dump(label)
	
type HighestLinesByFile = {Filename:string; LineCount:int; Location:Hyperlinq}

let getHighestLinesByFile threshold = 
	summaries 
	|> Seq.sortBy (fun x-> -x.LineCount - 1) 
	|> (fun fs -> if useTakeLimits.IsSome then Seq.take(useTakeLimits.Value) fs else fs)
	|> Seq.map (fun x -> {Filename=x.Filename;LineCount=x.LineCount;Location=makeLinq x.FullPath x.RelativePath })

buildLimitString getHighestLinesByFile "HighestLines by file" highestLinesByFileMinimum

// -------------- highest lines by folder ---------------

type HighestLinesByFolderDetails = { Filename:string; LineCount:int; Nonspaces:int}

type HighestLinesByFolder = { Path:string;TotalLines:int;Details:seq<HighestLinesByFolderDetails>}

let getHighestLinesByFolder threshold = 
	let getGroupLineCount (items:FileSummary seq) :int = 
		items 
		|> Seq.map(fun fs -> fs.LineCount)
		|> Seq.sum
	let filter (key:string,items:FileSummary seq) = 
		(getGroupLineCount items) > threshold
	summaries 
	|> Seq.groupBy (fun x->x.RelativePath) 
	|> (fun group -> if useThresholds then (Seq.filter filter group) else group )
	|> (fun group -> if useTakeLimits.IsSome then Seq.take(useTakeLimits.Value) group else group)
	|> Seq.map (fun (key,items) -> (key, items |> Seq.sumBy (fun i->i.LineCount) , items)) 
	|> Seq.sortBy (fun (key,l,items)-> -l) 
	
	|> Seq.map (fun (key, l, items) -> 
		{Path=key;TotalLines=l;Details=
			(items |> Seq.map (fun i ->{Filename= i.Filename;LineCount=i.LineCount;Nonspaces=i.Nonspaces.Value}))})


buildLimitString getHighestLinesByFolder "Highest lines by folder" highestLinesByFolderMinimum
	 

// -------------- highest lines by file base ---------------

type FilenameDetail = {
	Lines:int;
	FileName:string;
	Nonspaces:int;
	RelativePath:string;
	}
	
type FilenameGrouping = { 
	File:Object;
	Lines:int;
	Nonspaces:int;
	FilenameDetails:FilenameDetail seq;
	
	}
	
let getHighestByFileBase threshold = 
	let asFilenameGrouping (key,summaries:FileSummary seq) = 
		{ 
			FilenameGrouping.File = key
			Lines = (summaries |> Seq.map (fun x-> x.LineCount) |> Seq.sum)
			Nonspaces = summaries |> Seq.map (fun x-> x.Nonspaces.Value) |> Seq.sum
			FilenameDetails = 
				summaries 
				|> Seq.map (fun (x:FileSummary)-> {FilenameDetail.FileName = x.Filename; Lines = x.LineCount; RelativePath = x.RelativePath;Nonspaces =  x.Nonspaces.Value})
				|> Seq.sortBy (fun x-> x.RelativePath)
			}
			
	let getGroupLineCount (items:FileSummary seq) :int = 
		items 
		|> Seq.map(fun fs -> fs.LineCount)
		|> Seq.sum
	let filter (key:string,items:FileSummary seq) = 
		(getGroupLineCount items) > threshold
		
	summaries
	|> Seq.filter (fun f -> f.Filename.Contains("."))
	|> Seq.groupBy (fun f-> f.Filename.Substring(0,f.Filename.IndexOf('.'))) //before '.'
	|> (fun group -> if useThresholds then (Seq.filter filter group) else group )
	|> Seq.map asFilenameGrouping
	|> Seq.sortBy (fun f-> -f.Lines)
	|> (fun group -> if useTakeLimits.IsSome then Seq.take(useTakeLimits.Value) group else group)
	
buildLimitString getHighestByFileBase "Highest lines by file base" highestLinesByFileMinimum


// -------------- highest magic by file ---------------

type MagicByFile = { RelativePath:string; Filename:string;PotentialMagicNumbers:int;DoubleQuotes:int;LineCount:int; Nonspaces:int}

let getHighestMagicByFile threshold = 
	let magic (fileSummary:FileSummary) = fileSummary.PotentialMagicNumbers.Value + fileSummary.DoubleQuotes.Value / 2
	summaries
	|> Seq.sortBy (fun fs -> -(magic fs))
	|> (fun group -> if useTakeLimits.IsSome then Seq.take(useTakeLimits.Value) group else group)
	
buildLimitString getHighestMagicByFile "Highest magic by file" highestMagicByFileMinimum

()