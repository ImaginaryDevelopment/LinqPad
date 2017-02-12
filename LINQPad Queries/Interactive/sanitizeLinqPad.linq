<Query Kind="FSharpProgram" />

// sensitize/desensitize
// consider: looking for the the connection tag only in the first 3 lines of the text, otherwise it is not a file that needs to be cleaned


// this script should do one of two things depending on usage/input
// 1. walk all files in a linqpad directory hierarchy, pull sensitive info into files that match some ignore pattern, overwrite existing files for clean commits/diffing
// 2. resensitize for usage

let dumpReverse :  (obj) -> unit =
    let dc = DumpContainer()
    dc.Dump() |> ignore
    (fun o -> 
        match dc.Content with
        | :? (obj list) as items -> List.Cons(o,items)
        | _ -> [ o ]
        |> fun content -> dc.Content <- content
    )

type Rail<'TSuccess> =
    | Success of 'TSuccess
    // for items that aren't an error, but also don't need anything done
    | NoOp
    | Failure of string
    with override x.ToString() = sprintf "%A" x

let unsanitizedExtension = ".pri"

let getUnsanitizedPath queryPath = 
    let dir = Path.GetDirectoryName queryPath
    let name = Path.GetFileNameWithoutExtension queryPath
    Path.Combine(dir,name + unsanitizedExtension)
let mutable maxIndexOfConnectionText = 0
let mutable maxLineOfConnectionText = 0
let toDump = ResizeArray<_>()
let sanitize (q:ObjectModel.Query) = 
    // for safety don't allow overwriting if there is a sanitization already present.
    let target = getUnsanitizedPath q.FilePath
    if File.Exists target && (File.ReadAllText target) = (File.ReadAllText q.FilePath) then
        File.Delete target
    elif File.Exists target then
        let diffIt = 
            let toRun () = Util.Cmd(sprintf "\"C:\Program Files\KDiff3\kdiff3.exe\" \"%s\" \"%s\"" q.FilePath target, true) |> ignore
            LINQPad.Hyperlinq(toRun,"diff")
        let deleteIt =
            let del () = File.Delete(target)
            LINQPad.Hyperlinq(del,"delete")
        
        toDump.Add(q.FilePath,diffIt,deleteIt)
    if q.FilePath = Util.CurrentQueryPath then
        NoOp
    elif File.Exists target then
        sprintf "File Exists %s" target |> Failure
    else
        // if we are going to sanitize, just backup the whole file? or sanitized parts?
        
        // what all stuff do we clean?
        //<UserName>...</UserName>
        //<Password>...</Password>
        // or entire <Connection> section?
        
        let text = File.ReadAllText q.FilePath

        let shouldSanitize =
            // clean absolute reference paths ?
            text.Contains("<Connection>") && q.FilePath <> Util.CurrentQueryPath
        if shouldSanitize then
            printfn "Sanitizing"
            (
                printfn "updating statistics"
                let i = text.IndexOf("<Connection>")
                if i > maxIndexOfConnectionText then
                    maxIndexOfConnectionText <- i
                let line = text.SplitLines() |> Seq.findIndex(fun l -> l.Contains("<Connection>"))
                if line > maxLineOfConnectionText then
                    maxLineOfConnectionText <- line
            )
            
            File.Copy(q.FilePath, target)
            try
                
                (text |> before "<Connection>") + (text |> after "</Connection>")
                |> fun c -> File.WriteAllText(q.FilePath, c)
            with ex ->
                // delete backup on failure
                File.Delete target
                text.Dump("failing")
                reraise()
        
        
        Success target
        
let desanitize (q:ObjectModel.Query) =
    let target = getUnsanitizedPath q.FilePath
    if File.Exists target then
        File.Copy(target, q.FilePath, overwrite=true)
        File.Delete(target)
    target
// sanitize, pause for commits, unsanitize immediately
try
    Util.GetMyQueries()
    |> Seq.map sanitize
    |> Seq.sort
    |> List.ofSeq
    |> List.rev
    |> Dump
    |> ignore
with ex -> 
    ex.Dump()
maxLineOfConnectionText.Dump("Max line")
maxIndexOfConnectionText.Dump("max index")
Util.ReadLine<string>("Do your Commits now, then we'll desanitize") |> ignore

toDump.Dump()

Util.GetMyQueries()
|> Seq.map desanitize
|> dumpt "deleted backups"
|> ignore