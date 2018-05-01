<Query Kind="FSharpProgram" />

// iterate a build drop structure, getting a specific file's version info
// consider: adding the ability to query changeset number
let target = Environment.ExpandEnvironmentVariables("%droproot%")

let subPath = @"drop\PM\PracticeManagement.exe"

Directory.EnumerateDirectories target
|> Seq.choose(fun d ->
    let fp = Path.Combine(d,subPath)
    if File.Exists fp then
        Some fp
    else None
)
|> Seq.sort
|> List.ofSeq
|> List.rev
|> List.truncate 20
|> List.map(fun fp ->
    let fi = FileInfo(fp)
    let fvi = FileVersionInfo.GetVersionInfo fp
    fp,fvi.FileVersion,fi.CreationTime, fi.LastWriteTime
)
|> Dump
|> ignore