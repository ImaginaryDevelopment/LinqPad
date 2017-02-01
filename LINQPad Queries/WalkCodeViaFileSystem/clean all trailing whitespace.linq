<Query Kind="FSharpProgram" />

// trim all trailing whitespaces from current and child directories
let dumpt t x = x.Dump(description=t); x
let targetDirectories = [
    //@"c:\tfs"; 
    @"C:\projects\FsInteractive\MacroRunner"]
let dirBlacklist = [".sonarqube";"bin";".nuget";".sonar";".vs";"obj";"$tf";".git";"packages";"Debug";"temp"]
let toReadableCharPairs (s:string) = 
    s.ToCharArray()
    |> Seq.map (fun ch ->
        ch, (Convert.ToByte ch |> fun x -> x.ToString("x2")), int ch
    )
let regReplaceCount pattern replacementPattern txt =
    let mutable count = 0
    let result = 
        Regex.Replace(txt,pattern,evaluator=
            (fun m -> 
                count <- count + 1
                let result = m.Result replacementPattern
                //let result = Regex.Replace(m.Value, pattern, replacement= replacementPattern)
                if not <| Regex.IsMatch(m.Value,pattern) then
                    m.Value
                    |> toReadableCharPairs
                    |> dumpt "match input"
                    |> ignore
                    result
                    |> toReadableCharPairs
                    |> dumpt "match result"
                    |> ignore
                    failwithf "inner result didn't match pattern"
                if txt = result || txt.Length = result.Length then
                    failwithf "m.Result failure"
                result
            ), options=RegexOptions.Multiline
        )
    if result = txt && count <> 0 then
        printfn "charCount(old:%i;new:%i)" txt.Length result.Length
        txt.Dump("old text")
        result.Dump("result")
        failwithf "text comparison is bad (%i)" count
    if txt<>result && count < 1 then
            failwithf "count or text compare is bad"
    result, count
    
let stripWhiteSpace (fp:string) = 
    File.ReadAllText fp
    |> fun txt-> txt, regReplaceCount @"[ \t]+(\r?$)" "$1" txt
    |> fun (txt,(newTxt,count)) -> 
        if txt = newTxt && count <> 0 then
            printfn "charCount(old:%i;new:%i)" txt.Length newTxt.Length
            txt.Dump("old text")
            newTxt.Dump("new text")
            failwithf "text comparison is bad (%i,%s)" count fp
        if txt<>newTxt && count < 1 then
            failwithf "count or text compare is bad"
        if txt <> newTxt then
            File.WriteAllText(fp, newTxt, Encoding.UTF8)
            printfn "Wrote out new text to %s" fp
        count
        
type DirPath = |DirPath of string
let (|GetDir|) (x:DirPath) = function |DirPath d -> d
let sidePrint s x = printfn "%s" s; x
module Unused =
    type IsBinaryResult = 
        | DoesntExist
        | IsDirectory
        | IsEmpty
        | IsBinary of length:int64
        | IsText of length:int64
        
    // most code will be taken from http://stackoverflow.com/a/26652983/57883
    let isBinaryFile path = 
        match File.Exists path, Directory.Exists path with
        | false, true -> IsDirectory 
        | false, false -> DoesntExist
        | true, _ -> 
            match FileInfo(path).Length with
            | 0L -> IsEmpty
            | len -> 
                let isBinary = 
                    use stream = new StreamReader(path)
                    stream 
                    |> Seq.unfold(fun str -> 
                        match stream.Read() with 
                        | -1 -> None
                        | ch -> Some(ch, str)
                    )
                    |> Seq.cast<char>
                    |> Seq.exists System.Char.IsControl
                if isBinary then IsBinary len else IsText len
type EnumerationFailedResult = 
    | Unauthorized of UnauthorizedAccessException
    | PathTooLong of PathTooLongException
type EnumerationResult = 
    | Enumerated of DirPath*string list
    | EnumFail of EnumerationFailedResult
let tryEnumerateDirectoryFiles filepattern root= 
    //printfn "Getting files in directory %s" root
    try
        let result = 
            if String.IsNullOrWhiteSpace filepattern then
                Directory.EnumerateFiles(root) 
            else 
                Directory.EnumerateFiles(root, filepattern) 
            |> List.ofSeq
            
        //printfn "Found %i file(s)" result.Length
        
        Enumerated (DirPath root,result)
         
    with 
    | :? UnauthorizedAccessException as ex ->
        ex.Data.["path"] <- root
        Unauthorized ex |> EnumFail
    | :? PathTooLongException as ex ->
        ex.Data.["path"] <- root
        PathTooLong ex |> EnumFail

    
let rec findAllFiles filePatterns root: EnumerationResult seq = 
    if dirBlacklist |> Seq.exists(fun black -> Path.GetFileName root = black) then
        Seq.empty
    else
        filePatterns
        |> Seq.map (fun filePattern ->
        
            seq {
                //printfn "Enumerating %s" root
                
                yield tryEnumerateDirectoryFiles filePattern root 
                yield! Directory.EnumerateDirectories root |> Seq.map (findAllFiles [filePattern]) |> Seq.collect id
            }
        )
        |> Seq.collect id
type ReplacementInfo = {FilePath:string; Count:int}
targetDirectories
|> Seq.map (findAllFiles ["*.fs";"*.cs"])
|> Seq.collect id
// we don't want dirs where nothing was found filtered when looking for directories it shouldn't have looked in at all
//|> Seq.filter(function | Enumerated (_,[] -> false | _ -> true)
|> sidePrint "about to list it"
|> List.ofSeq
|> sidePrint "listed"
|> fun x -> 
    let len = x.Length
    x
    |> Seq.mapi (fun i x -> (x,i))
    |> Seq.fold(fun (dirs:DirPath list,items:string list,failures:EnumerationFailedResult list) (e,i) -> 
        Util.Progress <- Nullable (i * 100 / len)
        match e with
        | Enumerated (d,e) -> [d]@dirs,e@items,failures
        | EnumFail f -> dirs,items, [f]@failures
    ) ([],[],[])
|> fun (dirs,items,fails) ->
    printfn "done folding"
    
//    Util.Progress <- Nullable 0
//    items
//    |> Seq.take 1
    items
    |> Seq.map (fun fp ->
        let replacementCount = stripWhiteSpace fp
        {FilePath=fp;Count=replacementCount}
    )
    |> Dump
    
    dirs |> Seq.map (function |DirPath d -> d) |> fun x -> x.Dump("directories") |> ignore
    if fails.Length > 0 then
        fails.Dump()
//|> Dump
|> ignore