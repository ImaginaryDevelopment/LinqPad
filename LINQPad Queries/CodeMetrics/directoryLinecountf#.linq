<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Namespace>System.Collections.ObjectModel</Namespace>
  <Namespace>System.IO</Namespace>
  <Namespace>System.Runtime.InteropServices</Namespace>
  <Namespace>System.Windows.Forms</Namespace>
</Query>

open System
//open System.Diagnostics
open System.IO
//open System.Windows.Forms

let doTestFileExclude = false
let highestLinesByFileMinimum = 550
let highestLinesByFolderMinimum = 500
let highestMagicByFileMinimum = 6

let useThresholds = true
let useTakeLimits = Some(20)

type HighestLinesByFile = {Location:Hyperlinq;Filename:string; LineCount:int;Nonspaces:int;DoubleQuotes:int;PotentialMagicNumbers:int}
type HighestLinesByFolderDetails = { Filename:string; LineCount:int; Nonspaces:int; MaxLineLength:int; MaxLineIndex:int; LongLines:int}

type HighestLinesByFolder = { Path:string;TotalLines:int;Details:seq<HighestLinesByFolderDetails>}

//let after (value:string) (delimiter:string) = 
let endsWithIgnore (test:string) (value:string) = value.EndsWith(test,StringComparison.CurrentCultureIgnoreCase)
let startsWithIgnore (test:string) (value:string) = value.StartsWith(test,StringComparison.CurrentCultureIgnoreCase)
let fileExcludeEndings = ["designer.cs";"generated.cs";"codegen.cs"]

let fileExclude	(a:string):bool = 
    endsWithIgnore "designer.cs" a ||
    startsWithIgnore "jquery-" a ||
    startsWithIgnore "AssemblyInfo" a ||
    endsWithIgnore "generated.fs" a
    
    
let pathExcludeEndings = ["obj"; "Debug";".sonar";"ServerObjects";"Service References";"Web References";"PackageTmp";"TestResults";"packages";"$tf";".git";"bin" ]

let pathExclude (a:string) :bool =
    List.exists ( fun elem -> endsWithIgnore elem a) pathExcludeEndings ||
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


module BrowseFileDialogReflection = 
    module Reflections = 
        let c_flags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic
        let windowsFormsAssembly = typeof<FileDialog>.Assembly
        let fosPickFoldersBitFlag = windowsFormsAssembly.GetType("System.Windows.Forms.FileDialogNative+FOS").GetField("FOS_PICKFOLDERS").GetValue(null) :?> uint32
        let iFileDialogType = windowsFormsAssembly.GetType("System.Windows.Forms.FileDialogNative+IFileDialog")
        let private createVistaDialogMethodInfo = typeof<OpenFileDialog>.GetMethod("CreateVistaDialog", c_flags)
        let createVistaDialog o = createVistaDialogMethodInfo.Invoke(o, Array.empty)
        let private onBeforeVistaDialogMethodInfo = typeof<OpenFileDialog>.GetMethod("OnBeforeVistaDialog", c_flags)
        let onBeforeVistaDialog o iFileDialog = onBeforeVistaDialogMethodInfo.Invoke(o, [| iFileDialog |]) |> ignore<obj>
        let private getOptionsMethodInfo = typeof<FileDialog>.GetMethod("GetOptions", c_flags)
        let private setOptionsMethodInfo = iFileDialogType.GetMethod("SetOptions", c_flags)
        let getOptions o = getOptionsMethodInfo.Invoke(o, Array.empty) :?> uint32
        let setOptions iFileDialog (pickFoldersBitFlag: uint32) = setOptionsMethodInfo.Invoke(iFileDialog, [| pickFoldersBitFlag|]) |> ignore<obj>
        let private adviseMethodInfo = iFileDialogType.GetMethod("Advise")
        let advise iFileDialog adviseParametersWithOutputConnectionToken = adviseMethodInfo.Invoke(iFileDialog, [| adviseParametersWithOutputConnectionToken; 0u|]) |> ignore<obj>
        let private vistaDialogEventsConstructorInfo = windowsFormsAssembly.GetType("System.Windows.Forms.FileDialog+VistaDialogEvents").GetConstructor(c_flags, null, [| typeof<FileDialog> |] , null)
        let vistaDialogEvents o = vistaDialogEventsConstructorInfo.Invoke([| o |])
        let show x (owner:IntPtr) = iFileDialogType.GetMethod("Show").Invoke(x, [| owner|]) :?> int
    
    open Reflections 
    let showOpenFolderDialog owner initialDirectory title = 
        use ofd = new OpenFileDialog(AddExtension = false, CheckFileExists = false, DereferenceLinks=true, Filter = "Folders|\n", InitialDirectory = initialDirectory, Multiselect=false, Title=title)
        let iFileDialog = createVistaDialog ofd
        //iFileDialog.GetType().Dump() <- returned on win10 System.Windows.Forms.FileDialogNative+FileOpenDialogRCW
        onBeforeVistaDialog ofd iFileDialog
        (getOptions ofd ||| fosPickFoldersBitFlag)
        |> setOptions iFileDialog
    
        let adviseParametersWithOutputConnectionToken = vistaDialogEvents ofd
        advise iFileDialog adviseParametersWithOutputConnectionToken
        let result = 
            try
                let retVal = show iFileDialog owner
                Some((retVal = 0), ofd.FileName)
            with ex ->
                None
        //ofd.ShowDialog()
        result
    
//    showOpenFolderDialog IntPtr.Zero null null 
//    |> Dump
//    |> ignore
        
    
//instance of above type
let currentSettings:CountSettings=	{
    Path=
        let userPath=
            match Util.ReadLine("SourceDirectory?(blank to browse for the folder)",@"%devroot%") with
            | null | "" -> 
                let devRoot = Environment.ExpandEnvironmentVariables("%devroot%")
                printfn "%%devroot%%=%s" devRoot
                let initialDir = match String.IsNullOrEmpty devRoot with | true -> null | false -> devRoot
                match BrowseFileDialogReflection.showOpenFolderDialog IntPtr.Zero initialDir "Selected the desired target folder/project folder/sln folder" with
                | Some(false,_) | None -> failwithf "No directory selected"
                | Some (true, p) -> p
            | x -> Environment.ExpandEnvironmentVariables x
                
        let userExpanded= if userPath.Contains('%') then System.Environment.ExpandEnvironmentVariables(userPath) else userPath
        let exists=System.IO.Directory.Exists(userExpanded)
        if not exists then //guard clause
            raise(DirectoryNotFoundException(userExpanded))
        do userExpanded.Dump("Searching")
        userExpanded
    Patterns=["*.cs";"*.aspx";"*.ascx";"*.fs"]
    FileExclude=fileExclude
    PathExclude=pathExclude
    }
    
module Helpers =
    let (|RMatch|_|) p x = 
        let m = Regex.Match(x,p)
        if m.Success then
            Some m
        else None
       
    let (|ValueString|_|) =
        function
        | null | "" -> None
        | x when String.IsNullOrWhiteSpace x -> None
        | x -> Some <| ValueString x
        
    let (|Whitespace|_|) = 
        function
        | null | "" -> None
        | x when String.IsNullOrWhiteSpace x -> Some Whitespace
        | _ -> None
        
open Helpers

module LongestType = 
    let (|TypeDeclaration|Line|) =
        function
        | null | "" | Whitespace -> Line 0
        | RMatch "^(\s*)(type (.+)\(.*\)\s*=)?" m ->
            match m.Groups.[2].Value with
            | "" 
            | null -> Line m.Groups.[1].Length
            | _ -> 
                TypeDeclaration (m.Groups.[1].Length,m.Groups.[3].Value)
        // match on anything that's not an empty line or a comment line
        | RMatch "^(\s*)[^/]+" m ->
            Line m.Groups.[1].Length
        | x -> failwithf "bad pattern? %s" x
    
    type TypeInfo = {TypeStartIndentation:int;Lines:int;TypeDeclarationText:string; TypeStartLine:int}
    type LineWalkState =
        | NotInAType
        | InAType of TypeInfo
        
        
    type LongestInfo = {LineIndex:int;Length:int;EndLineIndex:int}    
    let getOptMax (li,typeLength, eLi) =
        let def = {LineIndex=li; Length=typeLength;EndLineIndex=eLi}
        function
        | Some y -> if y.Length > typeLength then y else def
        | None -> def
        >> Some
    let startType debug (lineIndex,lineIndent,typeText,typeEndIndex) =
        if debug then
            printfn "Starting type %s" typeText
        InAType {TypeStartIndentation=lineIndent; Lines=1;TypeDeclarationText=typeText;TypeStartLine=lineIndex}
        
    let foldLines debug (currentState:LineWalkState,longestOpt: LongestInfo option) (lineIndex,line) = 
        // if we aren't in a type then just carry longest type length option forward
        // if we are starting a type then 
        match currentState,line with
        | NotInAType, Line lineIndentation ->
            if debug then 
                printfn "skipping line %s" line
            NotInAType, longestOpt
        | NotInAType, TypeDeclaration (ti,x) ->
            startType debug (lineIndex,ti,x,lineIndex), getOptMax (lineIndex,1, lineIndex) longestOpt
        | InAType ti, Line lineIndentation ->
            if lineIndentation <= ti.TypeStartIndentation then // type definition is over
                if debug then
                    printfn "Ending type(%s) on line %i with line %s" ti.TypeDeclarationText lineIndex line
                NotInAType, getOptMax (ti.TypeStartLine,ti.Lines,lineIndex) longestOpt
            else
                if debug then
                    printfn "Continuing type %s" line
                let x = ti.Lines + 1
                InAType {ti with Lines = x}, getOptMax (ti.TypeStartLine,x,lineIndex) longestOpt
        
        | InAType ti, TypeDeclaration (li,x) ->
            // what to do when the indentation = type's indentation or is less, but there's no text? that doesn't end a type
            if li <= ti.TypeStartIndentation then // type definition is over
                if debug then
                    printfn "Ending type(%s) with line %s" ti.TypeDeclarationText line
                startType debug (lineIndex,li,x,lineIndex), getOptMax (ti.TypeStartLine,ti.Lines,lineIndex) longestOpt
            else
                if debug then
                    printfn "Continuing type %s" line
                let x = ti.Lines + 1
                InAType {ti with Lines = x}, getOptMax (ti.TypeStartLine,x, lineIndex) longestOpt
    //    | x,y -> (x,y).Dump("fail"); invalidOp (sprintf "%A" x)
        
    
    let getLongestType debug maxLineIndex = 
        Seq.fold (foldLines debug) (NotInAType, None)
        >> function
            |InAType ti, longestOpt ->
                getOptMax (ti.TypeStartLine,ti.Lines,maxLineIndex) longestOpt
            | NotInAType, longestOpt -> 
                longestOpt 
                
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
    let prepend="~" + if relativePath.StartsWith("\\") then "" else "\\" 
    let lines = lazy(fullPath |> readerFunc)
    let text = lazy(lines.Value |> String.concat String.Empty)
    let lengthMetrics = lazy(
        lines.Value |> Seq.mapi(fun i x -> i, String.length x)
    )
    member self.FullPath with get() = fullPath
    member self.Filename with get() = System.IO.Path.GetFileName(self.FullPath)
    member self.RelativePath with get() = prepend+relativePath.Substring(0,relativePath.Length-self.Filename.Length)
    
    member self.LineCount = 
        let x = lines.Value |> Seq.length
        if x <= 1 then printfn "Bad line count %i for %s" x fullPath
        x
    member self.Nonspaces=lazy(text.Value |> Seq.filter (fun x-> Char.IsWhiteSpace x <>true) |> Seq.length)
    member self.DoubleQuotes=lazy(text.Value |> Seq.filter (fun x-> '"'=x) |> Seq.length)
    member self.PotentialMagicNumbers=lazy(text.Value |> rgNumber.Matches |> fun x->x.Count)
    // the longestLine's length
    member self.MaxLineLength=lazy(lengthMetrics.Value |> Seq.map snd |> Seq.max) //self.AllLines.Value |> Seq.map String.length |> Seq.max)
    // the longestLine's index
    member self.MaxLineIndex=lazy(lengthMetrics.Value |> Seq.maxBy snd |> fst)
    member self.LongLines=lazy(lengthMetrics.Value |> Seq.map snd |> Seq.filter ((>) 80) |> Seq.length) //self.AllLines.Value |> Seq.map String.length |> Seq.filter((>) 80) |> Seq.length)
    // only doing F# for now, especially since it is the one more likely to have many classes in the same file
    member self.LongestType = lazy(
            printfn "LineCount is %i" self.LineCount
            lines.Value |> Seq.mapi(fun i x -> (i,x)) |> LongestType.getLongestType false (self.LineCount - 1)
        )
    
    
let asSummary (files:string[]) :seq<FileSummary> =
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
let linesByFileExtension = 
    summaries 
    |> Seq.filter(fun e -> e.Filename.Contains ".generated." |> not ) 
    |> Seq.filter(fun e -> e.RelativePath.Contains("Pm.ViewModelsC") |> not)
    |> Seq.groupBy(fun e -> Path.GetExtension e.Filename) 
    |> Seq.map (fun (fe,items) -> 
        let lineCount = items |> Seq.map(fun i -> i.LineCount) |> Seq.sum
        fe, lineCount, items) 
    |> fun x -> x.Dump("by extension")

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
    try
        f(threshold).Dump(label)
    with ex -> 
        ex.Data.Add("title",title)
        ex.Data.Add("threshold",threshold)
        reraise ()
    

let getHighestLinesByFile threshold = 
    summaries 
    |> Seq.sortBy (fun x-> -x.LineCount - 1) 
    |> (fun fs -> if useTakeLimits.IsSome then Seq.take(useTakeLimits.Value) fs else fs)
    |> Seq.map (fun x -> {  Filename=x.Filename
                            LineCount=x.LineCount
                            Location=makeLinq x.FullPath x.RelativePath
                            Nonspaces=x.Nonspaces.Value
                            PotentialMagicNumbers=x.PotentialMagicNumbers.Value
                            DoubleQuotes= x.DoubleQuotes.Value

                        })

buildLimitString getHighestLinesByFile "HighestLines by file" highestLinesByFileMinimum

// -------------- highest type length file --------------

summaries
|> Seq.filter(fun x -> x.Filename.EndsWith(".fs"))
|> Seq.map(fun x -> 
    let l,li,eli = 
        match x.LongestType.Value with
        | Some x-> x.Length, x.LineIndex, x.EndLineIndex
        | None -> 0,0,0
    x.Filename, l, sprintf "%i..%i" li eli
)
|> Seq.sortBy(fun (_,l,_) -> -l)
|> Seq.truncate 20
|> List.ofSeq
|> fun x -> x.Dump("highest type length fs file")
|> ignore

// -------------- highest lines by folder ---------------



let getHighestLinesByFolder threshold = 
    let getGroupLineCount (items:FileSummary seq) :int = 
        items 
        |> Seq.map(fun fs -> fs.LineCount)
        |> Seq.sum
    let filter (key:string,items:FileSummary seq) = 
        (getGroupLineCount items) > threshold
    summaries 
    |> Seq.groupBy (fun x-> x.RelativePath)
    |> (fun group -> if useThresholds then (Seq.filter filter group) else group )
    |> (fun group -> if useTakeLimits.IsSome then Seq.take(useTakeLimits.Value) group else group)
    |> Seq.map (fun (key,items) -> (key, items |> Seq.sumBy (fun i->i.LineCount) , items)) 
    |> Seq.sortBy (fun (key,l,items)-> -l) 
    
    |> Seq.map (fun (key, l, items) -> 
        {Path=key;TotalLines=l;Details=
            (items |> Seq.map (fun i ->
                {
                    Filename= i.Filename
                    LineCount=i.LineCount
                    Nonspaces=i.Nonspaces.Value
                    MaxLineLength=i.MaxLineLength.Value
                    LongLines=i.LongLines.Value
                    MaxLineIndex=i.MaxLineIndex.Value
                }
            ))})

try
    buildLimitString getHighestLinesByFolder "Highest lines by folder" highestLinesByFolderMinimum
with ex -> ex.Dump()
     

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
    |> (fun group -> if useThresholds then (Seq.filter filter group) else group)
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