<Query Kind="FSharpProgram" />

// sensitize/desensitize
// sanitize appears to work! 

// this script should do one of two things depending on usage/input
// 1. walk all files in a linqpad directory hierarchy, pull sensitive info into files that match some ignore pattern, overwrite existing files for clean commits/diffing
// 2. resensitize for usage

// let's see how well Util.GetMyQueries() does before deciding to skip it and find them manually
let _myQueriesPath () = 
    // desired: switch to looking for the highest parent directory of anything from getMyQueries where the folder name = "LINQPad Queries"
    // faulty: assumes there are entries in the root linqpad queries directory
    LINQPad.Util.GetMyQueries()
    |> Seq.map (fun q -> q.FilePath)
    |> Seq.fold(fun (shortestFilePath:string option) (nextFilePath:string) -> 
                                    match shortestFilePath |> Option.map (fun fp -> fp.Length < nextFilePath.Length) |> Option.getOrDefault false with
                                    | true ->  shortestFilePath
                                    | false -> Some nextFilePath
                                    ) None
    |> Option.map Path.GetDirectoryName
    |> fun x -> x.Dump()
    @"C:\projects\LinqPad\LinqPad\LINQPad Queries\"

type Rail<'TSuccess> =
    | Success of 'TSuccess
    | Failure of string
    
let _getQueries() = Directory.GetFiles( _myQueriesPath () , "*.linq", SearchOption.AllDirectories)

let unsanitizedExtension = ".pri"

let getUnsanizitedPath queryPath = 
    let dir = Path.GetDirectoryName queryPath
    let name = Path.GetFileNameWithoutExtension queryPath
    Path.Combine(dir,name + unsanitizedExtension)
    
let sanitize (q:ObjectModel.Query) = 
    // for safety don't allow overwriting if there is a sanitization already present.
    let target = getUnsanizitedPath q.FilePath
    if File.Exists target then
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
            text.Contains("<Connection>")
        if shouldSanitize then
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
let desanitize _ =()        
match Util.ReadLine<_>("Sanitize?") with
| true ->
    
    Util.GetMyQueries()
    |> Seq.map sanitize
    |> Dump
    |> ignore
| false ->
    Util.GetMyQueries()
    |> Seq.map desanitize
    |> Dump
    |> ignore
    
()


    