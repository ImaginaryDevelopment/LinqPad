<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Namespace>System.IO</Namespace>
</Query>

open System
//open System.Diagnostics
open System.IO
//open System.Windows.Forms
let doTestFileExclude = false

//fileExclude= Func<string,bool> a=>
let fileExclude	(a:string):bool = 
	a.EndsWith("designer.cs",StringComparison.CurrentCultureIgnoreCase)||
	a.StartsWith("jquery-",StringComparison.CurrentCultureIgnoreCase)||
	a.StartsWith("AssemblyInfo",StringComparison.CurrentCultureIgnoreCase)
	
let pathExclude (a:string) :bool =
	a.EndsWith("obj") ||
	a.EndsWith("Debug") ||
	a.EndsWith("bin",StringComparison.CurrentCultureIgnoreCase) ||
	a.EndsWith(".sonar") ||
	a.EndsWith("ServerObjects") ||
	a.EndsWith("Service References")||
	a.EndsWith("Web References") ||
	a.EndsWith("PackageTmp") ||
	a.EndsWith("TestResults") ||
	a.EndsWith("packages") ||
	a.Contains(@"\Scripts\Mvc3")

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
		let userPath=Util.ReadLine("SourceDirectory?",@"%PAYSPANROOT%")
		let userExpanded= if userPath.Contains('%') then System.Environment.ExpandEnvironmentVariables(userPath) else userPath
		let exists=System.IO.Directory.Exists(userExpanded)
		if not(exists) then //guard clause
			raise(DirectoryNotFoundException(userExpanded))
		do userExpanded.Dump("Searching")
		userExpanded
	Patterns=["*.cs";"*.js";"*.aspx";"*.ascx"]
	FileExclude=fileExclude
	PathExclude=pathExclude
	}
	
//set cwd (not a functional call, imperative?)
System.Environment.CurrentDirectory<-currentSettings.Path

let rec getDirectories (basePath:string) dirFilter= seq{
	for d in Directory.EnumerateDirectories(basePath) do
		if not(dirFilter d) then
			yield d
			yield! getDirectories d dirFilter
	}
	
let includedDirectories=getDirectories currentSettings.Path currentSettings.PathExclude

includedDirectories |> Seq.length |> fun x->x.Dump("directories included")
//includedDirectories |> Seq.toArray |> Dump
let getFilesByPatterns directories patterns =
	seq{
		for d in directories do
		for pattern in patterns do
			for file:string in Directory.EnumerateFiles(d,pattern) do
				yield file
	}

let allFiles = getFilesByPatterns includedDirectories currentSettings.Patterns

allFiles |> Seq.length |> fun x->x.Dump("Total files matching pattern list found")



//rec means recursive function
let filterFiles files fileFilter= seq{
	for file in files do
		let filename=System.IO.Path.GetFileName(file)
		if not(fileFilter(filename)) then
			yield file
	}
	
let filterFilesResult= filterFiles allFiles currentSettings.FileExclude |> Seq.toArray
//filterFilesResult.GetType().Dump()			
filterFilesResult |> Seq.length |> fun x->x.Dump("Total files included")

type FileSummary(relativePath:string, fullPath:string,readerFunc:string->string[]) as self = 
	let rgNumber = new Regex(@"\.?[0-9]+(\.[0-9]+)?", RegexOptions.Compiled)
	let prepend="~"+if relativePath.StartsWith("\\") then "" else "\\" 
	member self.FullPath with get() = fullPath
	member self.Filename with get() = System.IO.Path.GetFileName(self.FullPath)
	member self.RelativePath with get() = prepend+relativePath.Substring(0,relativePath.Length-self.Filename.Length)
	member private self.AllLines=lazy(self.FullPath |> readerFunc)
	member private self.AllText=lazy( self.AllLines.Value |> String.concat "")
	member self.LineCount =lazy( self.AllLines.Value |> Seq.length)
	member self.Nonspaces=lazy(self.AllText.Value |> Seq.filter (fun x->Char.IsWhiteSpace(x)<>true) |> Seq.length)
	member self.DoubleQuotes=lazy(self.AllText.Value |> Seq.filter (fun x-> '"'=x) |> Seq.length)
	member self.PotentialMagicNumbers=lazy(self.AllText.Value |> rgNumber.Matches |> fun x->x.Count)
	
let  asSummary (files:string[]) :seq<FileSummary> =
	
	let uriPath (r:string)= if r.Length>1 then "~"+r.Substring(1) else "" //if relPath is .
	let reader x = System.IO.File.ReadAllLines(x)
	seq{
	
		for file:string in files do
			let relPathWithFilename=file.Substring(currentSettings.Path.Length)//.Dump()
			//do file.Dump(relPath)
			let summary=new FileSummary(relPathWithFilename,file,reader)
			yield summary
	}

let summaries = asSummary filterFilesResult

let aSummary=summaries.First()


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
	//LINQPad.Util.RawHtml(rawHtml(dirPath))
type HighestLinesByFile = {Filename:string; LineCount:int; Location:Hyperlinq}
let getHighestLinesByFile = summaries |> Seq.sortBy (fun x-> -x.LineCount.Value - 1) |> 
	Seq.take(20) |>
	Seq.map (fun x -> {Filename=x.Filename;LineCount=x.LineCount.Value;Location=makeLinq x.FullPath x.RelativePath }) // List.map//|> Seq.map (fun x-> x.FileName)

getHighestLinesByFile.Dump("HighestLines by file")

type HighestLinesByFolderDetails = { Filename:string; LineCount:int; Nonspaces:int}

type HighestLinesByFolder = { Path:string;TotalLines:int;Details:seq<HighestLinesByFolderDetails>}

let getHighestLinesByFolder = 
	summaries |> Seq.groupBy (fun x->x.RelativePath) |> 
	Seq.map (fun (key,items) -> (key, items |> Seq.sumBy (fun i->i.LineCount.Value) , items)) |>
	Seq.sortBy (fun (key,l,items)-> -l) |>
	Seq.map (fun (key, l, items) -> {Path=key;TotalLines=l;Details=
		(items |> Seq.map (fun i ->{Filename= i.Filename;LineCount=i.LineCount.Value;Nonspaces=i.Nonspaces.Value}))})
		

getHighestLinesByFolder |> Seq.take(10) |> fun x->x.Dump("Highest lines by folder")
//
//getHighestLinesByFile summaries |> fun x-> x.Dump("Highest by file")
	

type FilenameDetail = {
	Lines:int;
	FileName:string;
	RelativePath:string;
	Nonspaces:int;
	}
type FilenameGrouping = { 
	File:Object;
	Lines:int;
	Nonspaces:int;
	FilenameDetails:seq<FilenameDetail>;
	//FilenameDetails Seq of FilenameDetail;
	}

0


////demonstrating access modifier	
//let private testFileExclude() =
//	let testCases = ["test";"test.designer.cs";"test.Designer.cs"]
//	let mapped = testCases |>
//		List.map(fun (fn:string) ->
//			(fn,currentSettings.FileExclude(fn))
//		)
//	mapped.Dump("testFileExclude done")
//	
//if doTestFileExclude then
//	do testFileExclude()