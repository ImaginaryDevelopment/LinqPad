// use packet to download nuget packages for script reference
// boilerplate code from http://www.navision-blog.de/blog/2014/10/14/retrieving-github-download-counts/


//------------------------------------------
// Step 0. Boilerplate to get the paket.exe tool
 
open System
open System.IO
 
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
 
if not (File.Exists "paket.exe") then
    let url = "http://fsprojects.github.io/Paket/stable"
    use wc = new Net.WebClient() in let tmp = Path.GetTempFileName() in let stable = wc.DownloadString(url) in wc.DownloadFile(stable, tmp); File.Move(tmp,Path.GetFileName stable)
 
// Step 1. Resolve and install the packages 
 
#r "paket.exe"
 
Paket.Dependencies.Install """
    source https://nuget.org/api/v2
    nuget FSharp.Data 
    nuget FSharp.Charting
""";;
 
// Step 2. Use the packages 
 
#r "packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#load "packages/FSharp.Charting/FSharp.Charting.fsx"

open FSharp.Data
open FSharp.Charting


type GitHubReleases = JsonProvider<"https://api.github.com/repos/fsprojects/Paket/releases">

let releases =
    let rec loop acc page =
        let releases = GitHubReleases.Load(sprintf "https://api.github.com/repos/fsprojects/Paket/releases?page=%d" page)
        if releases <> [||] then loop (Array.append acc releases) (page + 1) else acc

    loop [||] 1


let downloadCounts = 
    [for release in releases do
        for asset in release.Assets do
            if asset.Name = "paket.exe" then
                yield release.Name,asset.DownloadCount]

    |> List.sortBy snd
    |> List.rev

let allDownloads = downloadCounts |> List.sumBy snd

Chart.Bar (List.take 10 downloadCounts)