<Query Kind="FSharpProgram" />

// find all symbols that are not mentioned again anywhere in the app
// appears to be working as intended
// needs to account for string literals

// not accounting for private (nor .xaml + .xaml.cs + .designer.cs pairings)
// not accounting for single letter variables/functions/symbols
// not accounting for '
// not accounting for "
let path = Environment.ExpandEnvironmentVariables("%devroot%")
module Helpers =
    let flip f x y = f y x
    let endsWithIgnore (test:string) (value:string) = value.EndsWith(test,StringComparison.CurrentCultureIgnoreCase)
    let startsWithIgnore (test:string) (value:string) = value.StartsWith(test,StringComparison.CurrentCultureIgnoreCase)
    let delimit (d:string) (x:string seq) = String.Join(d,Array.ofSeq x)
()
open Helpers
let filePatterns=["*.cs";"*.fs";"*.xaml"]
let fileExcludeEndings = ["designer.cs";"generated.cs";"codegen.cs"]

let fileExclude	(a:string):bool = 
    endsWithIgnore "designer.cs" a ||
    startsWithIgnore "jquery-" a ||
    startsWithIgnore "AssemblyInfo" a ||
//    endsWithIgnore "generated.fs" a ||
    endsWithIgnore "generated.cs" a
    
    
let pathExcludeEndings = ["obj"; "Debug";"node_modules";".sonar";"ServerObjects";"Service References";"Web References";"PackageTmp";"TestResults";"packages";"$tf";".git";"bin" ]

let pathExclude (a:string) :bool =
    List.exists ( fun elem -> endsWithIgnore elem a) pathExcludeEndings
    || a.Contains @"NonSln"
    || a.Contains ".localhost"
    || a.Contains "Generated_C#_Source"
    || a.Contains "Test"
    || a.Contains "FakeConsole"
    || a.Contains "Meffers"
    || a.Contains "LobExporter"
    
    
let rec getDirectories (basePath:string) dirFilter= seq{
    for d in Directory.EnumerateDirectories(basePath) do
        if not(dirFilter d) then
            yield d
            yield! getDirectories d dirFilter
    }

    
let getFilesByPatterns directories patterns =
    seq{
        for d in directories do
        for pattern in patterns do
            for file:string in Directory.EnumerateFiles(d,pattern) do
                yield file
    }

let includedDirectories=getDirectories path pathExclude

    
let files = 
    let allFiles = getFilesByPatterns includedDirectories filePatterns
    
    let filterFiles files fileFilter= seq{
        for file in files do
            let filename=System.IO.Path.GetFileName(file)
            if not(fileFilter(filename)) then
                yield file
        }
    filterFiles allFiles fileExclude |> Seq.toArray

type CodeType =
    | Cs
    | Fs
    | Xaml
    
let getCodeType x =
    if x |> endsWithIgnore ".cs" then Cs
    elif x |> endsWithIgnore ".fs" then Fs
    elif x |> endsWithIgnore ".xaml" then Xaml
    else raise <| NotImplementedException()
    
let filterSymbols = 
    let csFsShared = ["public"; "namespace";"static";"if";"null";"return";"new";"String";"string";"internal";"int";"as"]
    function
    | Cs -> ["using";"System";"List";"class";"void";"var";"this"]@csFsShared
    | Fs -> ["open";"module";"type";"list";"let";"isNull";"obj"]@csFsShared
    | Xaml -> ["xmlns";"Property";"Value";"Setter";"Style";"Name";"Key"]


let insertOrAddCountMap key x m = 
    let v = 
        if Map.containsKey key m then
            m.[key] + x
        else x
    Map.add key v m
    
// recurse on symbols that contain qualifications/member access System.String or foo.bar()
let rec getSymbols ct (lines:string seq) =
//    let isOpenOrUsing x = match ct with | Xaml -> false | Cs -> Regex.Match(x,@"^\s*using .*;").Success | Fs -> Regex.Match(x,"^\s*open ").Success
    let filter: string -> bool = flip Seq.contains (filterSymbols ct) >> not
    // from https://stackoverflow.com/questions/11925305/regex-negative-look-behind-anywhere-on-line https://stackoverflow.com/a/11926350/57883
    let notComment = "(?<!//.*?)"
    // fail! this is eliminating all items on a line with quotes
//    let notQuoted = sprintf "(?<!\"[^\"]*?)%s(?!\")"
//    let regularSymbol = notQuoted "[A-Za-z_][\w_0-9.]+" // (?<!\"[^\"]*?)[\w_][\w0-9.]+(?!\")
    // allowing variables that start with _
    let regularSymbol = "[A-Za-z_][\w_0-9]+" // (?<!\"[^\"]*?)[\w_][\w0-9.]+(?!\")
    let tickSymbol = "``[^`]+``"
    //  ((?<!"[^"]*?)[A-Za-z_][\w_0-9.]+(?!")|``[^`]+``)
    let symbols = sprintf "(%s|%s)" regularSymbol tickSymbol
    
    lines
//    |> Seq.filter (isOpenOrUsing>>not)
    |> delimit "\r\n"
    |> fun text -> Regex.Matches(text,sprintf "%s%s" notComment symbols)
    |> Seq.cast<Match> 
    |> Seq.map (fun m -> m.Value)
//    |> Seq.collect(fun x -> if x.Contains "." then getSymbols ct (x.Split([| "." |],StringSplitOptions.None)) else [x])
    |> Seq.filter filter
    |> List.ofSeq

let countSymbols = 
        Seq.fold(fun (mapByCount:Map<string,int>) (sym:string) ->
            let i = 
                if Map.containsKey sym mapByCount then
                       mapByCount.[sym] + 1
                else 1
            Map.add sym i mapByCount 
        ) Map.empty
        
let findUnusedSymbols files = 
    //[@"C:\tfs\practicemanagement\trunk\Pm.Dal\CHelpers.fs";@"C:\tfs\practicemanagement\trunk\PracticeManagement.Foundation\DataAccess\XpressEHRDocumentDataAccess.cs"]
    //[@"C:\tfs\practicemanagement\trunk\PracticeManagement.Foundation\DataAccess\XpressEHRDocumentDataAccess.cs"]
    // get all symbols by file by extension
    files
    |> Seq.map(fun fullPath ->
        // all symbols not preceeded by //
        let ct = getCodeType fullPath
        let allSymbols :string list = 
            File.ReadAllLines fullPath
            |> getSymbols ct
        let symbolsByCount = countSymbols allSymbols
        Path.GetExtension fullPath, fullPath, symbolsByCount
    )
    |> Seq.fold(fun r (_,fp,x) ->
            x
            |> Map.toSeq
            |> Seq.fold(fun (symbolMap:Map<string,string>,symbolCounts:Map<string,int>) (sym:string,count:int) -> // : Map<string,string>*Map<string,int> 
                let s1 = Map.add sym fp symbolMap
                let s2 = insertOrAddCountMap sym count symbolCounts 
                s1,s2
                
            ) r
    ) (Map.empty,Map.empty)
//     remove things that have more than 1 reference
    |> fun (m1,m2) ->
        m1
        |>  Map.filter(fun k v ->
            m2.[k] = 1)
        // we let the generated files come through so that regular files' references to them don't get flagged as being lone references
        // then remove them at the end because we don't care about ones that only are referenced in generation
        |> Map.filter(fun _ v -> not <| endsWithIgnore "generated.fs" v)
        
    |> Map.toSeq
    |> Seq.sortBy snd
    
let getSample() = 
    let ct,targetFile = Fs,@"C:\tfs\practicemanagement\trunk\Pm.Dal\AdoHelper.fs"
    let singleFileSample () = 
        File.ReadAllLines targetFile
        |> Seq.map(fun x ->  x, getSymbols ct [x] )
        |> Seq.filter(fst >> (fun x -> x.Contains "cstring"))
    let singleFileCountSample() =
        File.ReadAllLines targetFile
        |> getSymbols ct
        |> countSymbols
    singleFileCountSample()

findUnusedSymbols files
//getSample()
|> Dump 
|> ignore