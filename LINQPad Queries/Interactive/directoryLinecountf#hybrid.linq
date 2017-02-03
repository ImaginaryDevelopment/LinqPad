<Query Kind="FSharpProgram" />

let userPath=Util.ReadLine("SourceDirectory?",@"%DEVROOT%")
let filePatterns=["*.cs";"*.js";"*.aspx";"*.ascx"]
let startsWithI (d:string) (x:string) = x.StartsWith(d, StringComparison.InvariantCultureIgnoreCase)
let endsWithI (d:string) (x:string) = x.EndsWith(d, StringComparison.InvariantCultureIgnoreCase)
let equalsI (d:string) (x:string) = x.Equals(d,StringComparison.InvariantCultureIgnoreCase)
let lift f x = f x; x
let fileExclude (a:string):bool = 
    let startsWithI d = startsWithI d a
    let endsWithI d = endsWithI d a
    let ignorePrefixes = [
        "jquery-"
        "AssemblyInfo"
        "react-"
        "jquery-"
    ]
    let ignoreSuffixes =[
        "designer.cs"
        "generated.cs"
    ]
    let ignoredFiles = [
        "bootstrap.js"
    ]
    ignorePrefixes |> Seq.exists startsWithI ||
    ignoreSuffixes |> Seq.exists endsWithI ||
    ignoredFiles |> Seq.exists (equalsI a)
    
    
     
let pathExclude (a:string) : bool =
    let ignoredFolders = [
        "obj" 
        "Debug" 
        "bin" 
        ".sonar" 
        "ServerObjects" 
        "Service References"
        "Web References" 
        "PackageTmp" 
        "TestResults" 
        "packages" 
        "node_modules"
        "$tf"
        "react"
    ]
        
    let shouldExclude = 
        ignoredFolders |> Seq.exists (equalsI (Path.GetFileName a)) ||
        a.Contains(@"\Scripts\Mvc3")
    shouldExclude


module Types = 
    type CountSettings = {Path:string; Patterns: string list; FileExclude: string -> bool; PathExclude: string -> bool}
    type FileSummary = {RelativePathWithFilename:string; File:string; LineCount: int } with
        member x.RelativePath = Path.GetDirectoryName x.RelativePathWithFilename
        
let currentSettings:Types.CountSettings={
    Path=
        let userExpanded= if userPath.Contains("%") then System.Environment.ExpandEnvironmentVariables(userPath) else userPath
        let exists=System.IO.Directory.Exists(userExpanded)
        if not(exists) then //guard clause
            raise(DirectoryNotFoundException(userExpanded))
        do userExpanded.Dump("Searching")
        userExpanded
    Patterns=filePatterns
    FileExclude=fileExclude
    PathExclude=pathExclude
    }
System.Environment.CurrentDirectory<-currentSettings.Path

let rec getDirectories (basePath:string) dirFilter= seq{
    for d in Directory.EnumerateDirectories(basePath) do
        if not <| dirFilter d then
            yield d
            yield! getDirectories d dirFilter
    }
    
let includedDirectories=getDirectories currentSettings.Path currentSettings.PathExclude

includedDirectories |> Seq.length |> fun x->x.Dump("directories included")

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


open Types    
let  asSummary (files:string[]) :seq<Types.FileSummary> =
    
    let uriPath (r:string)= if r.Length>1 then "~"+r.Substring(1) else "" //if relPath is .
    let reader x = System.IO.File.ReadAllLines(x)
    seq{
    
        for file:string in files do
            let relPathWithFilename=file.Substring(currentSettings.Path.Length)//.Dump()
            //do file.Dump(relPath)
            let summary = {FileSummary.RelativePathWithFilename= relPathWithFilename;File=file;LineCount= reader file |> Seq.length}
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
let getHighestLinesByFile (files:FileSummary seq) threshold : FileSummary seq = 
    files 
    |> Seq.filter(fun fi -> fi.LineCount > threshold) 
    |> Seq.sortByDescending (fun fi -> fi.LineCount)
//public IEnumerable<FileSummary> GetHighestLinesByFile(IEnumerable<FileSummary> files)
//{
//    return files.Where(fi=>fi.Lines>HIGHEST_LINES_BY_FILE_MINIMUM). OrderByDescending (fi => fi.Lines);
//}
        
let highestLinesByFile= getHighestLinesByFile summaries 20 |> Seq.take 10 |> Seq.map (fun x -> makeLinq x.File x.RelativePathWithFilename, x.LineCount)

highestLinesByFile.Dump("HighestLines by file")

//let getHighestLinesByFolder (files:FileSummary seq) threshold : FileSummary seq = 
//    files
//    |> Seq.groupBy(fun 
//
//let highestLinesByFolder =HighestLinesByFolder.GetHighestLinesByFolder ( summaries)
//        
//
//highestLinesByFolder |> Seq.take(10) |> fun x->x.Dump("Highest lines by folder")