<Query Kind="FSharpProgram" />

// write .bat file, compile for exercism, and run tests
//idea: update docs to include much of this script: https://github.com/ImaginaryDevelopment/xfsharp/blob/AddNonVsInstallation/docs/INSTALLATION.md#without-visual-studio
open System.ComponentModel
open System.Runtime.CompilerServices


        
[<AutoOpen>]
module Settings = 
    let quiet = true
    let excercismPath = @"C:\projects\excercism.io"
    let excercise = "difference-of-squares"
    let userPath = Environment.GetFolderPath Environment.SpecialFolder.UserProfile
    // needed to run fsc easily
    let vsvarsBatPath = @"C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\Tools\vsvars32.bat"
    let nugetPath = @"C:\projects\MvcOdata\.nuget\nuget.exe"


let regexMatch p line = Regex.Match(line,p)
let trim (s:string) = s.Trim()

module Seq =
    let regexMatches p lines= 
        lines
        |> Seq.map (regexMatch p)
        |> Seq.filter(fun r -> r.Success)
        

// let this script run with or without LINQPad
#if LINQPad // Linqpad's define doesn't actually work properly, but demonstrates intent nicely, and should function should LINQPad ever fix this for F#
let dumpt (t:string) x = x.Dump t; x
#else
module Util =
    type CommandExecutionException(msg,exitCode) = 
        inherit Exception(if String.IsNullOrEmpty msg then String.Concat("The process returned an exit code of ",exitCode |> string) else msg)
        
        new(msg) = CommandExecutionException(msg,0)
        
        member x.ErrorText = msg
        member x.ExitCode :int = exitCode
        
        
//    let createPsi fn args = ProcessStartInfo(FileName=fn, Arguments = args, UseShellExecute=false, RedirectStandardOutput
    let private GetCmdProcess cmdText args useCmdExec = 
        let cmdText = trim cmdText
        let processStartInfo = 
            ProcessStartInfo(
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                RedirectStandardInput = true,
                UseShellExecute = false,
                CreateNoWindow = true)        
        if not useCmdExec then
            processStartInfo.FileName <- cmdText
            processStartInfo.Arguments <- args
            // translation of   
            // if ((!string.IsNullOrEmpty(args) || !cmdText.Contains<char>(' ') ? false : !File.Exists(cmdText)))
            let argsProvided = not <| String.IsNullOrEmpty args
            let hasSpace = cmdText.Contains(' ')
            //let  = argsProvided || not hasSpace
            let addArgs = if not argsProvided || not hasSpace then false else not <| File.Exists cmdText
            if addArgs then
                let strArrays = cmdText.Split(" ".ToCharArray(), 2, StringSplitOptions.RemoveEmptyEntries)
                if File.Exists strArrays.[0] then
                    processStartInfo.FileName <- strArrays.[0]
                    processStartInfo.Arguments <- strArrays.[1]
            try
                if Path.IsPathRooted processStartInfo.FileName then
                    let directoryName = Path.GetDirectoryName processStartInfo.FileName
                    if Directory.Exists directoryName then
                        processStartInfo.WorkingDirectory <- directoryName
            with | :? ArgumentException -> ()
        else
            processStartInfo.FileName <- "cmd.exe";
            let str = 
                if cmdText.Contains(' ') then
                    String.Concat("\"", cmdText, "\"")
                else cmdText
            processStartInfo.Arguments <- String.Concat("/c ", str)
            if not <| String.IsNullOrEmpty args then
                let processStartInfo1 = processStartInfo
                processStartInfo1.Arguments <- String.Concat(processStartInfo1.Arguments, " ", args)
        Process.Start processStartInfo
        
    let Cmd commandText args quiet = 
    //public static string[] Cmd(string commandText, string args, bool quiet)
            let mutable cmdProcess:Process = null
            let strs = new List<string>();
            try
                cmdProcess <- GetCmdProcess commandText args false
            with | :? Win32Exception as win32Exception ->
                cmdProcess <- GetCmdProcess commandText args true
            use proc = cmdProcess
            
            let stringBuilder = StringBuilder()
            cmdProcess.ErrorDataReceived.AddHandler (DataReceivedEventHandler(fun sender errorArgs -> stringBuilder.AppendLine errorArgs.Data  |> ignore))
            cmdProcess.BeginErrorReadLine()
            let mutable str = cmdProcess.StandardOutput.ReadLine()
            while (str |> isNull |> not) do
                if not quiet then Console.WriteLine(str)
                strs.Add str
                str <- cmdProcess.StandardOutput.ReadLine()
            let exitCode = cmdProcess.ExitCode
            if exitCode > 0 then
                raise <| CommandExecutionException(stringBuilder.ToString(), exitCode)
                
            strs.ToArray()
        
let dumpt (t:string) x = printfn "%s:%A" t x; x
[<Extension>]
type Extensions = 
    [<Extension>]
    static member Dump(x) = printfn "%A" x; x
    [<Extension>]
    static member Dump(x,s) = dumpt s x
#endif

    
let targetDir = Path.Combine(userPath,"exercism","fsharp",excercise) 
    
Environment.CurrentDirectory <- targetDir


let fsc outFile outType files references = 
    let files = 
        match files with 
        | [] -> String.Empty
        | x -> String.Join(" ", files)
    let references = 
        match references with
        | [] -> String.Empty
        | x -> x |> Seq.map(fun s -> sprintf "--reference:%s" s) |> fun x -> sprintf " %s" (String.Join(" ",x |> Array.ofSeq))
    sprintf "fsc --out:%s --target:%s %s%s" outFile outType files references
    
// nuget install nunit and/or nunit runner
let nugetCommand = 
    sprintf "\"%s\" install %s" nugetPath // https://docs.nuget.org/consume/command-line-reference

if not <| File.Exists vsvarsBatPath then
    failwithf "could not locate vsvars"
let toCompile = Directory.GetFiles(targetDir, "*.fs") |> Array.sortBy(fun p -> p.Contains("tests", StringComparison.InvariantCultureIgnoreCase)) |> List.ofArray
let batFile = "exercism.bat"
let targetDll = Path.GetFileName(targetDir) + ".dll"

[   "setlocal"
    sprintf "call \"%s\"" vsvarsBatPath
    nugetCommand "NUnit"
    nugetCommand "NUnit.ConsoleRunner"
    ]
|> dumpt "batch"
|> fun lines -> File.WriteAllLines(batFile,lines |> Array.ofSeq)

Environment.CurrentDirectory |> dumpt "Current directory" |> ignore

let outputs = Util.Cmd batFile null quiet
let nUnitPath, nUnitConsolePath = 
    let versions = 
        outputs
        |> Seq.regexMatches "'NUnit\.?(.*) (.*)'"
        |> Seq.map (fun m -> (m.Groups.[1].Value, m.Groups.[2].Value))
        |> dict
        //|> dumpt "batchfile1 results"
    
    Path.Combine(sprintf "NUnit.%s" versions.[""],"lib","net45","nunit.framework.dll"),
        Path.Combine(sprintf "NUnit.ConsoleRunner.%s" versions.["ConsoleRunner"], "tools","nunit3-console.exe")
        
// copy the nunit dll to the dll output directory which is just the current folder so that nunit console will function
File.Copy(nUnitPath,Path.GetFileName nUnitPath,true)
let referenceNuGet =
    let nUnitFolders = Directory.GetDirectories(Environment.CurrentDirectory,"NUnit.*")
    nUnitFolders
    //|> dumpt "NUnitFolders"
// append fsc with proper references to whatever nugetdownloaded
[   fsc targetDll "library" toCompile [nUnitPath]
    //http://stackoverflow.com/questions/11085822/how-to-execute-nunit-test-cases-from-command-prompt
    sprintf "%s --framework=v4.5 %s" nUnitConsolePath targetDll
    //"IF %ERRORLEVEL% NEQ 0 pause" // not a good idea if being called via Process.Start
]
|> fun lines -> File.AppendAllLines(batFile,lines)

let lines = Util.Cmd batFile null quiet

let testSummaryOpt = 
    lines
    |> Seq.regexMatches "Overall result: (.*)"
    |> Seq.tryHead
    |> Option.map (fun m -> m.Groups.[1].Value)
    
type TestDetail = {Count:int; Passed:int; Failed:int; Inconclusive:int;Skipped:int}
let testDetailsOpt = 
    lines
    |> Seq.regexMatches @"Test Count: (\d+), Passed: (\d+), Failed: (\d+), Inconclusive: (\d+), Skipped: (\d+)"
    |> Seq.tryHead
    |> Option.map (fun m -> {Count= int m.Groups.[1].Value; Passed=int m.Groups.[2].Value; Failed = int m.Groups.[3].Value;Inconclusive= int m.Groups.[4].Value; Skipped= int m.Groups.[5].Value})
   
match testSummaryOpt,testDetailsOpt with
| Some "Passed", Some details -> 
    details.Dump() |> ignore
    match Util.ReadLine<bool>("Publish excercism?") with
    | true -> 
        Environment.CurrentDirectory <- excercismPath
        Util.Cmd (sprintf "exercism submit %s" toCompile.[0]) null false
        |> dumpt "excercism submission output"
        |> ignore< string[] >
    | false -> ()
| x,y -> printfn "summary,detail:%A" (x,y)
// run nUnit command line reference: http://www.nunit.org/index.php?p=consoleCommandLine&r=2.2.10