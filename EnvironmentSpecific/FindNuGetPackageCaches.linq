<Query Kind="FSharpProgram" />

let localData = Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData

let after delimiter (t:string) = t.Substring( t.IndexOf(delimiter,StringComparison.CurrentCulture) + delimiter.Length)
	
let enumerateFiles path search searchOption = 
	Directory.EnumerateFiles(path,search,searchOption)
let linqpadPackagePaths = [
	Path.Combine [| localData;"LinqPad";"NuGet"|], SearchOption.AllDirectories
	Path.Combine [| localData;"NuGet";"Cache" |], SearchOption.TopDirectoryOnly
	]
let scrapeBasePath b (p: string seq) = 
	p
	|> Seq.map ( fun path -> after b path)
	
let pathToDirAndFile p = Path.GetDirectoryName p, Path.GetFileName p
linqpadPackagePaths 
|> Seq.map (fun (p,so) -> p, enumerateFiles p "*.nupkg" so |> scrapeBasePath p |> Seq.map pathToDirAndFile,so)
|> Dump
	