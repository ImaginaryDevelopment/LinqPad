<Query Kind="FSharpProgram" />

// get git branch list, checkout each branch, looking for X

let debug = false
module Option =
    let ofNullOrEmpty =
        function
        | null -> None
        | "" -> None
        | x -> Some x
    let ofValueString x =
        if String.IsNullOrWhiteSpace x then
            None
        else Some x
        
module String =
    let trim (x:string) =
        Option.ofNullOrEmpty x
        |> Option.map(fun x -> x.Trim())
        |> Option.defaultValue null
    
    let trimStart1 (delim:char) x =
        Option.ofNullOrEmpty x
        |> Option.map(fun x -> x.TrimStart(delim))
        |> Option.defaultValue null
    let startsWith (delim:string) x =
        Option.ofNullOrEmpty x
        |> Option.map(fun x -> x.StartsWith(delim))
        |> Option.defaultValue false
    let isValueString x = String.IsNullOrWhiteSpace x |> not
        
module Result =        
    let tryGetError =
        function
        | Ok _ -> None
        | Error e -> Some e
    let swallowErrors (x : Result<_,_> seq) =
        (List.empty, x)
        ||> Seq.fold(fun state item ->
            match item with
            | Error _ -> state
            | Ok item -> (item::state)
        )
module Async =
    let map f x =
        async {
            let! value = x
            return f value
        }
    let bind f x =
        async {
            let! value = x
            return! f value
        }
    
module Proc =
    open System.Diagnostics
    let runCaptured name args wdOpt =
        async {
        
            let captures = System.Collections.ObjectModel.ObservableCollection()
            let psi =
                ProcessStartInfo(
                    FileName = name,
                    CreateNoWindow = true,
                    UseShellExecute = false,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true
            )
            wdOpt
            |> Option.iter(fun wd ->
                psi.WorkingDirectory <- wd
            )
            
            args
            |> List.iter psi.ArgumentList.Add
            
            use p = new Process(StartInfo = psi)
            let capAdd f : DataReceivedEventHandler =
                DataReceivedEventHandler(
                    fun sender (args:DataReceivedEventArgs) ->
                        let value = f args.Data
                        try
                             captures.Add(value)
                        with | :? ArgumentException as aex -> 
                            (captures.Count,aex).Dump("failure to add")
                )
                
            p.OutputDataReceived.AddHandler(capAdd Ok)
            p.ErrorDataReceived.AddHandler(capAdd Error)
            p.Start() |> ignore<bool>
            p.BeginOutputReadLine()
            p.BeginErrorReadLine()
            do! Async.AwaitTask (p.WaitForExitAsync())
            // let things wrap up and resolve on other threads
            do! Async.Sleep 100
            if p.ExitCode <> 0 then
                return Error(sprintf "%s %A -> %i" name args p.ExitCode, captures)
            else return Ok captures
        }
    let runOrFail name args wdOpt =
        runCaptured name args wdOpt
        |> Async.map(
            function
            | Ok output ->
                try
                    if debug && output.Count > 0 then
                        output
                        |> Seq.choose Result.tryGetError
                        |> List.ofSeq
                        |> function
                            | [] -> ()
                            | x ->
                                try
                                    if x |> List.exists(String.isValueString) then
                                        x.Dump("Errors")
                                with _ ->
                                    eprintfn "failed inside"
                                    reraise()
                    output
                with _ ->
                    (name,args,wdOpt).Dump("failing runOrFail")
                    output.Count.Dump("fail count")
                    reraise()
            | Error (msg, output) ->
                output.Dump(msg)
                failwithf "Did not succeed"
            >> Result.swallowErrors
       )
        
        
let wd = @"C:\projects\binarydefense\fed"        

let branches = 
    Proc.runOrFail "git" ["branch"] (Some wd)
    |> Async.RunSynchronously
    |> Seq.choose Option.ofValueString
    |> Seq.map String.trim
    |> Seq.sortBy (String.startsWith "*")
    |> Seq.map(String.trimStart1 '*')
    |> Seq.map String.trim
    |> List.ofSeq
let getFsFiles (dirPath: string) =
    IO.Directory.EnumerateFiles(dirPath,"*.fs", System.IO.EnumerationOptions(RecurseSubdirectories= true))
let findInFile (delimiter: string) (path: string) =
    async {
        let! lines = Async.AwaitTask <| System.IO.File.ReadAllLinesAsync path
        return
            lines
            |> Seq.mapi(fun i line -> i, line)
            |> Seq.tryFind(fun (_,x) -> x.Contains(delimiter))
    }
// git branch --show-current
let runGitOrFail args = Proc.runOrFail "git" args (Some wd)

let getBranch () =
    runGitOrFail ["branch"; "--show-current"]
    |> Async.RunSynchronously
    |> Dump
    |> ignore

branches
|> Seq.map(fun branch ->
    System.Threading.Thread.Sleep(250)
    if debug then
        printfn "Starting %s" branch
    //runGitOrFail  ["reset"; "--hard"]
    //|> Async.RunSynchronously
    //|> Dump
    //|> ignore
    runGitOrFail ["checkout"; branch; "-f"]
    |> Async.RunSynchronously
    //|> fun x -> x.Dump(sprintf "checkout result:%s" branch)
    |> ignore
    if debug then
        getBranch()
    
    getFsFiles wd
    |> Seq.mapi(fun i fn ->
        findInFile "EMsg" fn
        |> Async.map(fun result ->
            result
            |> Option.map(fun (j,line) ->
                fn,i,line,j
            )
        )
        |> Async.Catch
    )
    //|> List.ofSeq
    |> Async.Parallel
    |> Async.RunSynchronously
    |> fun x -> x
    |> Array.choose(
        function
        | Choice1Of2 (Some x) -> Some x
        | Choice1Of2 None -> None
        | Choice2Of2 ex ->
            ex.Dump("Failing")
            failwithf "async failure: %s" ex.Message
        )
    |> List.ofSeq
    |> fun x -> branch, x
)
//|> Seq.truncate 3
|> List.ofSeq
|> Dump
|> ignore