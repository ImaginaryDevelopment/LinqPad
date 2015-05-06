<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

type System.String with
	member t.after (delimiter, [<Optional;DefaultParameterValue(null)>]?culture) = 
		let culture = defaultArg culture StringComparison.CurrentCulture
		t.Substring( t.IndexOf(delimiter,culture) + delimiter.Length)
		//t.Substring( t.IndexOf(delimiter,if culture.IsSome then culture.Value else StringComparison.CurrentCulture) + delimiter.Length)

"test".after("te") |> Dump
<@ fun (t:string) -> t.after("te") @> |> Dump

let after delimiter (t:string) = t.Substring( t.IndexOf(delimiter,StringComparison.CurrentCulture) + delimiter.Length)
let before delimiter (t:string) = t.Substring(0,t.IndexOf(delimiter,StringComparison.CurrentCulture))



let enumerateFiles path search searchOption = 
	Directory.EnumerateFiles(path,search,searchOption)
	
let localData = Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData
let linqpadPackagePaths = 
	[
		Path.Combine [| localData;"LinqPad";"NuGet"|], SearchOption.AllDirectories
		Path.Combine [| localData;"NuGet";"Cache" |], SearchOption.TopDirectoryOnly
	]
	|> Seq.filter (fun (p,so) -> Directory.Exists p)
	
let scrapeBasePath b (p: string seq) = 
	p
	|> Seq.map ( fun path -> after b path)
	|> Seq.map ( fun path -> 
		let filename = Path.GetFileNameWithoutExtension path
		if path.EndsWith(filename) then path |> before filename else path
		)
	
let pathToDirAndFile p = Path.GetDirectoryName p, Path.GetFileName p
linqpadPackagePaths 
|> Seq.map (fun (p,so) -> p, enumerateFiles p "*.nupkg" so |> scrapeBasePath p |> Seq.map pathToDirAndFile,so)
|> Dump
	