<Query Kind="FSharpProgram" />

let target = @"C:\tfs\practicemanagement\php\translation.fs"

let text = File.ReadAllText target
let ifLine = """^(\s+if contains "\d+")\s*then\s*\n"""
let matchLine = @"^(\s+)match\s+modifier\s+with\s*\n"
let defLine = @"^(\s+)\|\s*_ ->\s*(.*)\n"
let otherLine = """(^(\s+)\|\s*("\w+") ->\s*(.*)\n)+"""
let fEval (m:Match) =
    sprintf "%s then\r\n%smatch modifier with\r\n%s%s%s| _ -> %s" 
        m.Groups.[1].Value 
        m.Groups.[2].Value 
        m.Groups.[5].Captures.[0].Value
        m.Groups.[5].Captures.[1].Value
        m.Groups.[3].Value // spacing
        m.Groups.[4].Value
        
let replace() =  Regex.Replace(text, ifLine+matchLine + defLine + otherLine, fEval, RegexOptions.Multiline)
let matchR () = 
    Regex.Matches(text, ifLine+matchLine + defLine + otherLine, RegexOptions.Multiline)
    |> Seq.cast<Match>
    |> Seq.map(fEval)
replace()
|> fun x -> File.WriteAllText(target, x)
