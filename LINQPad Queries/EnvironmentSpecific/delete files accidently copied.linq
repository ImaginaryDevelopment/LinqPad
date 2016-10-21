<Query Kind="FSharpProgram" />

// undo accidently mass copy
// compare on just filename, nothing else seems remotely reliable (known copied files show up as different on other attempted filtering vectors)

let src = @"C:\Windows\System32"

let target = @"C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\PublicAssemblies"

Directory.GetFiles(target)

|> Seq.map(fun f -> Path.Combine(src,Path.GetFileName f), f)

|> Seq.filter (fst >> File.Exists)
//|> Seq.filter (fun (src,questionableTarget) -> FileInfo(src).LastWriteTimeUtc = FileInfo(questionableTarget).LastWriteTimeUtc)
|> Dump
|> Seq.iter (snd >> File.Delete)