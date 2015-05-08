<Query Kind="FSharpProgram">
  <Reference>C:\Projects\trunk\mine\CodeMetrics\CodeMetrics\bin\Debug\CodeMetrics.dll</Reference>
  <Namespace>CodeMetrics</Namespace>
</Query>

let userPath=Util.ReadLine("SourceDirectory?",@"%devroot%")
let filePatterns=["*.cs";"*.js";"*.aspx";"*.ascx"]
let fileExclude (a:string):bool = 
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
        if not(dirFilter d) then
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


    
let  asSummary (files:string[]) :seq<Types.FileSummary> =
    
    let uriPath (r:string)= if r.Length>1 then "~"+r.Substring(1) else "" //if relPath is .
    let reader x = System.IO.File.ReadAllLines(x)
    seq{
    
        for file:string in files do
            let relPathWithFilename=file.Substring(currentSettings.Path.Length)//.Dump()
            //do file.Dump(relPath)
            let summary=new Types.FileSummary(relPathWithFilename,file,reader)
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
        
let highestLinesByFile=HighestLinesByFile.GetHighestLinesByFile ( summaries , makeLinq,20 )

highestLinesByFile.Dump("HighestLines by file")

       

let highestLinesByFolder =HighestLinesByFolder.GetHighestLinesByFolder ( summaries)
        

highestLinesByFolder |> Seq.take(10) |> fun x->x.Dump("Highest lines by folder")