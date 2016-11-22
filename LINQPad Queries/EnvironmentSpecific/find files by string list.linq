<Query Kind="FSharpProgram" />

// find files that contain both strings

let searchRoot = @"C:\TFS\PracticeManagement\dev\PracticeManagement"

let searchTerms = ["_currentUserId"; "SessionVariables.Instance.CurrentUser"]

Directory.GetFiles(searchRoot, "*.cs", SearchOption.AllDirectories)
|> Seq.map(fun fp -> fp,File.ReadAllText fp)
|> Seq.filter (fun (_,t) -> searchTerms |> Seq.forall (fun term -> t.Contains(term)))
|> Seq.map fst
|> Dump