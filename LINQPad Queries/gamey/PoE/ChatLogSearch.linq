<Query Kind="FSharpExpression" />

// find PoE chat log, search for text perhaps

// for steam users they should be in the steam game installation folder
let commonSteamFolderNames = [
    "Steam"
    "SteamLibrary"
]

let streamLines (fp:string) =
    seq{
        use stream = new FileStream(fp,FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
        use sr = new StreamReader(stream)
        yield!
            sr
            |> Seq.unfold(fun sr ->
                if sr.EndOfStream then
                    None
                else
                    Some(sr.ReadLine(),sr)
            )
    }
System.IO.DriveInfo.GetDrives()
|> Seq.filter(fun d ->
    d.DriveType = System.IO.DriveType.Fixed
)
|> Seq.map(fun x -> x.RootDirectory)
|> Seq.fold(fun state rd ->
    state @ [
        yield! commonSteamFolderNames |> Seq.map(fun x -> Path.Combine(rd.FullName, x))
        yield! commonSteamFolderNames |> Seq.map(fun x -> Path.Combine(rd.FullName,"Games", x))
    ]
    
) List.empty
|> List.filter(Directory.Exists)
|> Seq.map(fun root -> Path.Combine(root,"steamapps","common","Path of Exile", "logs"))
|> Seq.filter(Directory.Exists)
|> Seq.map(fun logPath -> Path.Combine(logPath,"Client.txt"))
|> Seq.filter(File.Exists)
|> Seq.map(fun logPath ->
    let fi = FileInfo(logPath)
    fi,fi.LastWriteTime
)
//|> dump
// search newest first
|> Seq.sortByDescending snd
//|> Seq.map(fun (p,dt) ->
//    let buffer = Array.zeroCreate 25 // 12 lines on either side for context
//    let mutable foundTarget = None // keeping context becomes Some i where i is the current mod index into buffer
//    //let lines = File.ReadLines(p.FullName) // see also https://stackoverflow.com/questions/4273699/how-to-read-a-large-1-gb-txt-file-in-net
//    let lines = streamLines(p.FullName)
//    lines 
//    |> Seq.mapi(fun i l ->
//        let index = i % buffer.Length
//        index
//        match foundTarget with
//        | None ->
//            buffer.[index] <- l
//            if l.Contains("devoto",StringComparison.InvariantCultureIgnoreCase) then
//                foundTarget <- Some 0 // write 12 more lines
//            None
//        | Some contextCount when contextCount >= (buffer.Length - 1) / 2  ->
//            // at this point are we at the last contextual write, or the one before it?
//            if true then
//                printfn "assuming we have 1 context item left to write"
//                buffer.[index] <- l
//            Some (buffer[contextCount..] |> Seq.append buffer[0..contextCount] |> List.ofSeq)
//            foundTarget <- None
//        | Some cc ->
//            foundTarget <- Some (cc + 1)
//            None
//        i,l
//    )
//    |> Seq.truncate 100
//    |> List.ofSeq
//        
//)

|> fun x ->
    x.Dump()