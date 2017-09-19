<Query Kind="FSharpProgram" />

let HIGHEST_LINES_BY_FILE_MINIMUM = 550;
let HIGHEST_LINES_BY_FOLDER_MINIMUM = 2000;
let HIGHEST_MAGIC_BY_FILE_MINIMUM = 6;
module Helpers = 
    let ciComparer = System.StringComparison.CurrentCultureIgnoreCase
    let endsWithI delim (x:string) = x.EndsWith(delim, ciComparer)
    let startsWithI delim (x:string) = x.StartsWith(delim, ciComparer)
    let equalsI delim (x:string) = String.Equals(x,delim, ciComparer)
    module Option = 
        let fromBool x = if x then Some() else None
    let (|EndsWithI|_|) delim x =  x |> endsWithI delim |> Option.fromBool
    let (|StartsWithI|_|) delim x = x |> startsWithI delim |> Option.fromBool
    let (|ContainsI|_|) delim (x:string) = x.Contains(delim, comparerCI) |> Option.fromBool
    let (|ContainsIAnyOf|_|) delims (x:string) = delims |> Seq.exists(fun d -> x.Contains(d,ciComparer)) |> Option.fromBool
    let (|EqualsI|_|) delim x = equalsI delim x |> Option.fromBool
    
open Helpers

type CountSettings() =
    let path = Util.ReadLine("SourceDirectory?", @"%devroot%")
    let pathExpanded = if path.Contains "%" then System.Environment.ExpandEnvironmentVariables path else path
    let patterns = [ "*.cs"; "*.aspx"; "*.ascx"; "*.js"] //;*.aspx;*.ascx
    let fileExclude = 
        function
        | EndsWithI "designer.cs"
        | StartsWithI "jquery-"
        | StartsWithI "AssemblyInfo"
        | EndsWithI "generated.cs"
        | Contains "jquery"
        | EndsWithI ".js"
        | EqualsI "T4MVC.cs" -> true
        | _ -> false
    let pathExclude=
        function
        | ContainsIAnyOf 
            [
                "Service References"
                ".git"
                "Web References"
                "PackageTmp"
                @"\Database\" 
                @"\Scripts\Mvc3" 
                "jquery" 
                ".sonar" 
                ".nuget" 
                "packages"
                @"\obj\"
                @"\bin\"]
            -> true
        | _ -> false

    let specialHandling = [ "*.mrx" ]
    member __.Path = pathExpanded
    member __.Patterns = patterns
    member __.FileExclude = fileExclude
    member __.PathExclude = pathExclude
    
type FilenameDetail = {Lines:int; FileName:string; RelativePath:string; Nonspaces:int} 
type FilenameGroupingDisplay = { File:obj; Lines:int; Nonspaces:int; FilenameDetails: FilenameDetail seq}
type FilenameGrouping = { File:string; Lines:int; Nonspaces:int; FilenameDetails: FilenameDetail seq} with
    member x.ToDump() = {FilenameGroupingDisplay.File = Util.Highlight x.File;Lines= x.Lines; Nonspaces = x.Nonspaces; FilenameDetails= x.FilenameDetails}
//public struct FileSummary
type FileSummary = 
    {
        RelativePath:string
        FileName:string
        Nonspaces:int
        Lines:int
        DoubleQuotes:int
        PotentialMagicNumbers:int
    }

type StringGrouping = 
    {
        Key:string
        Lines:int
        Files:string seq
    }

module Strategy = 
    let getHighestLinesByFileBase (files:FileSummary seq) : IEnumerable<FilenameGrouping>  =
        files
        |> Seq.filter(fun f -> f.FileName.Contains("."))
        |> Seq.groupBy(fun f -> f.FileName.Substring(0, f.FileName.IndexOf('.')))
        |> Seq.filter(fun (_,v) -> v |> Seq.sumBy (fun x -> x.Lines) > HIGHEST_LINES_BY_FILE_MINIMUM)
        |> Seq.map (fun (k,v) -> {  FilenameGrouping.File=k
                                    Lines=v.Sum(fun x -> x.Lines)
                                    Nonspaces = v.Sum(fun x -> x.Nonspaces)
                                    FilenameDetails = 
                                        v 
                                        |> Seq.map (fun x -> {FilenameDetail.Lines= x.Lines; FileName= x.FileName;RelativePath= x.RelativePath;Nonspaces= x.Nonspaces})
                                        |> Seq.sortBy(fun fd -> fd.RelativePath)
                                        |> List.ofSeq
                            }
           )
        |> Seq.sortByDescending (fun fg -> fg.Lines)
    let getHighestMagicByFile (files: FileSummary seq) :IEnumerable<FileSummary> =
        files
        |> Seq.filter (fun fi -> fi.PotentialMagicNumbers + (fi.DoubleQuotes / 2) > HIGHEST_MAGIC_BY_FILE_MINIMUM)
        |> Seq.sortByDescending (fun fi -> fi.PotentialMagicNumbers + (fi.DoubleQuotes / 2));

    let getHighestLinesByFile (files:FileSummary seq) : IEnumerable<FileSummary>  = 
        files
        |> Seq.filter (fun fi -> fi.Lines > HIGHEST_LINES_BY_FILE_MINIMUM)
        |> Seq.sortByDescending(fun fi -> fi.Lines)
    let getByRelativePath(files: FileSummary seq) = //: IOrderedEnumerable<StringGrouping> =
        files
        |> Seq.groupBy(fun r -> r.RelativePath)
        |> Seq.filter (fun (_,v) -> v |> Seq.sumBy (fun x -> x.Lines) > HIGHEST_LINES_BY_FOLDER_MINIMUM)
        |> Seq.map (fun (k,v) -> {StringGrouping.Key = k; Lines = v |> Seq.sumBy (fun x -> x.Lines); Files = v |> Seq.map (fun x -> x.FileName)})
        |> Seq.sortByDescending(fun r -> r.Lines)
    
open Strategy
let directoriesSearched:HashSet<string> = new HashSet<string>()

let recurseLocation basePath (relPath:string) patterns = 
    let rgNumber = Regex(@"\.?[0-9]+(\.[0-9]+)?", RegexOptions.Compiled)
    let rec r (relPath:string) = 
    
        
        let uriPath = if relPath.Length > 1 then "~" + relPath.Substring(1) else String.Empty
        seq{
        
            yield! patterns 
                |> Seq.map (fun pattern ->
                    System.IO.Directory.GetFiles(System.IO.Path.Combine(basePath,relPath),pattern)
                    |> Seq.map(fun file ->
                        let lines = System.IO.File.ReadAllLines file
                        let nonspaces = lines |> Seq.sumBy(Seq.filter(Char.IsWhiteSpace >> not) >> Seq.length)
                        let dblQuotes = lines |> Seq.sumBy(Seq.filter((=) '"') >> Seq.length)
                        let magicNumbersRg = lines|> Seq.sumBy(rgNumber.Matches >> (fun r -> r.Count))
                        {   RelativePath = uriPath
                            FileName = Path.GetFileName file
                            Lines = lines.Length
                            Nonspaces = nonspaces
                            DoubleQuotes = dblQuotes
                            PotentialMagicNumbers = magicNumbersRg
                        }
                    )
                )
                |> Seq.concat
            let next = 
                System.IO.Directory.GetDirectories relPath
                |> Seq.map (fun dir ->
                        directoriesSearched.Add dir |> ignore
                        r dir
                    )
                |> Seq.concat
            yield! next
        }
    r relPath


let settings = CountSettings()
settings.Path.Dump("Searching")

if not <| System.IO.Directory.Exists settings.Path then
    ()
else
    System.Environment.CurrentDirectory <- settings.Path
    directoriesSearched.Clear()
    let allResults = recurseLocation settings.Path "." settings.Patterns |> List.ofSeq
    allResults.Count().Dump("Total files found");
    
    directoriesSearched.Dump(sprintf "DirectoriesSearched %i" directoriesSearched.Count)
    let filtered = 
        allResults 
        |> Seq.filter (fun r -> 
                            settings.FileExclude r.FileName = false
                            && settings.PathExclude(r.RelativePath) = false)
        |> List.ofSeq
    //filtered.GroupBy(f=>f.RelativePath).Dump();
    //filtered.Take(800).Dump();
    //filtered.Skip(800).Dump();
    //return;
    filtered.Count().Dump("Total files included");
    
    getHighestLinesByFile(filtered).Dump(sprintf "Highest lines by file > %i" HIGHEST_LINES_BY_FILE_MINIMUM)
    
    getByRelativePath(filtered).Dump(sprintf "highest lines by folder > %i" HIGHEST_LINES_BY_FOLDER_MINIMUM)
    
    getHighestLinesByFileBase(filtered).Dump(sprintf "highest lines by filebase > %i" HIGHEST_LINES_BY_FILE_MINIMUM)
    filtered.First().Dump();
    getHighestMagicByFile(filtered).Dump(sprintf "Highest magic by file > %i" HIGHEST_MAGIC_BY_FILE_MINIMUM)
    //Util.HorizontalRun(true,new{Title="Highest lines by file",groupedByFilename},new{Title=,groupedByRelativePath}, filtered).Dump();
