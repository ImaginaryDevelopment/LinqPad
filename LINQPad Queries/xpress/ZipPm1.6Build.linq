<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.IO.Compression.FileSystem.dll</Reference>
</Query>

// currently assumes a build has already run
let targetDir = @"C:\tfs\pm2022\bin"
let outputDir = @"C:\tfs\drop"

let (|SEndsWith|_|) (delimiter:string) (text:string) = if isNull text || not <| text.EndsWith(delimiter) then None else Some ()
let (|SContains|_|) (delimiter:string) (text:string) = if isNull text || not <| text.Contains(delimiter) then None else Some ()

module Seq =
    let sortByDesc f s = Enumerable.OrderByDescending( s, Func<_,_>(f))
for f in Directory.GetFiles(targetDir, "*.txt") do
    match f with
    | SContains "DebugLog"
    | SEndsWith "App.Config" -> File.Delete(f)
    | _ -> ()
    
for f in Directory.GetFiles(targetDir, "*.xml") do
    match f with
    | SEndsWith "PracticeManagement.Config.Xml" -> File.Delete f
    | _ -> ()


    
type BuildVersionInputs = {Subversion:int; Year:int; Month:int; Day:int; BuildNumber:int}

let targetFilename bvi = 
    sprintf "Pm_1.6.%02i_%i%02i%02i_%i.zip" bvi.Subversion bvi.Year bvi.Month bvi.Day bvi.BuildNumber
    

let latest = 
    Directory.GetFiles(outputDir,"*.zip")
    |> Seq.sortByDesc File.GetCreationTimeUtc
    |> Seq.tryHead
    |> function
        | None -> None 
        | Some h -> 
            let m = Regex.Match(h, @"Pm_1.\d.([0-9]+)_([0-9]{4})([0-9]{2})([0-9]{2})_([0-9]+)\.zip")
            if m.Success then
                {
                    Subversion= m.Groups.[1].Value |> int 
                    Year = m.Groups.[2].Value |> int
                    Month = m.Groups.[3].Value |> int
                    Day = m.Groups.[4].Value |> int
                    BuildNumber = m.Groups.[5].Value |> int
                }
                |> Some
            else None

latest.Dump("latest")
let bvi =
    match latest with
    | Some bvi -> 
        if bvi.Year = DateTime.Today.Year && bvi.Month = DateTime.Today.Month && bvi.Day = DateTime.Today.Day then
            {bvi with Subversion = bvi.Subversion + 1; BuildNumber = bvi.BuildNumber + 1}
        else {Subversion = bvi.Subversion + 1; BuildNumber = 1; Year= DateTime.Today.Year; Month= DateTime.Today.Month; Day =DateTime.Today.Day }
    | None -> {Subversion = 1; BuildNumber = 1; Year= DateTime.Today.Year; Month = DateTime.Today.Month; Day = DateTime.Today.Day }
    
let outputFile = Path.Combine(outputDir, targetFilename bvi)

System.IO.Compression.ZipFile.CreateFromDirectory(targetDir, outputFile)

outputFile.Dump("Created zip at " + outputFile)