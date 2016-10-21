<Query Kind="FSharpProgram" />

// find the longest file.x
let combine childPath parentPath = Path.Combine(parentPath, childPath)
let countLines path = File.ReadLines path |> Seq.length

let directory = 
    Environment.ExpandEnvironmentVariables "%devroot%"
    |> combine "PracticeManagement\dev\PracticeManagement"
Directory.EnumerateFiles(directory, "*.cs", SearchOption.AllDirectories)
|> Seq.filter(fun f -> not <| f.EndsWith(".g.cs") && not <| f.EndsWith(".Designer.cs") && not <| f.EndsWith(".generated.cs"))
|> Seq.map(fun f-> f, countLines f)
|> Seq.sortBy (snd >> (*) -1)
|> Seq.take 6
|> Dump