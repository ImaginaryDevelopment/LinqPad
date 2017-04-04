<Query Kind="FSharpExpression" />

// find solutions that include a specific project
let regexMatch p x = Regex.Match(x,p)
let regexValue (m:Match) = m.Value
let regexValues (m:Match) = [ 1 .. m.Groups.Count - 1] |> Seq.map (fun i -> m.Groups.[i].Value) |> List.ofSeq
let regexMatchValues p x = 
    let m = regexMatch p x
    regexValues m, x

let projectLineRegex = 
    let quoted s = sprintf "\"%s\"" s
    let parenthesizedRegCapture = quoted (sprintf @"([^""]+)")
    let quotedGuidCapture = quoted (sprintf @"{([\w\d\-]+)}")
    // @"Project\(""{([\w\d\-]+)}""\)\s*=\s*""([^""]+),.*"
    // }""\)\s*=\s*""([^""]+),.*"
    sprintf @"Project\(%s\)\s*=\s*%s,\s*%s,\s*%s" quotedGuidCapture parenthesizedRegCapture parenthesizedRegCapture quotedGuidCapture
let targetDir = @"E:\payspan"
let targetProject = @"E:\payspan\all-apps\hpxapps\bundledpayment\PaySpan.Bpa.Website\PaySpan.Bpa.Website.csproj"
Directory.GetFiles(targetDir, "*.sln", SearchOption.AllDirectories)
|> Seq.filter (fun sln -> File.ReadAllText sln |> fun lines -> lines.IndexOf(Path.GetFileName targetProject)>=0)
|> Seq.map (fun slnPath -> slnPath, slnPath |> File.ReadAllLines |> Seq.filter (fun l -> l.StartsWith("Project")) |> List.ofSeq)
|> Seq.map (fun (slnPath,projectLines) -> slnPath, (projectLines |> List.map (regexMatchValues projectLineRegex)))
|> Seq.sortBy (snd >> List.length)


