<Query Kind="FSharpProgram" />

//ScanLogFiles

let buildAgentDir = @"\\build2010\C$\builds"
let interestedProject= "PracticeManagement"

let after (delimiter:string) (s:string) = s.Substring(s.IndexOf delimiter + delimiter.Length)
let before(delimiter:string) (s:string) = s.Substring(0,s.IndexOf delimiter)
let split (delimiter:char) (s:string) = s.Split([| delimiter |] )
let remove (delimiter:string) (s:string) = s.Replace(delimiter,String.Empty)
let trim (s:string) = s.Trim()

type BuildFindStrategy = | Latest |LatestBuild of buildname:string |All of buildname:string

module Seq = 
    let iterCont f x=
        x|> Seq.iter f
        x
        
let buildStrategy = LatestBuild "PracticeManagementRW"

let matchAndRemainder pattern matchF (l:string) = 
    let match' = Regex.Match(l,pattern)
    let lineAfter = [ match'] |> Seq.fold (fun (state:string) m -> state.Replace(m.Value, String.Empty)) l
    lineAfter,match' |> matchF
    
let matchesAndRemainder pattern matchF (l:string) = 
    let matches = Regex.Matches(l,pattern)|> Seq.cast<Match>
    let lineAfter = matches |> Seq.fold (fun (state:string) m -> state.Replace(m.Value, String.Empty)) l
    lineAfter,matches |> Seq.map matchF
    
type Rop<'a> =
    |Success of 'a
    |Failure of string
    
let bind f1 x = match x with | Success data -> f1 data | Failure s -> Failure s
let bind' f1 x = match x with | Success data -> f1 data |> Success | Failure s -> Failure s
    
let tryReadLines path = 
    try
        File.ReadLines(path)
        |> Success
    with | :? IOException as ex -> Failure ex.Message
    
type FscCall = {TargetObject:string; FscPath:string; References:string list; Files:string list; Remainder:string; (* Whole:string *) }

type BuildMessage =
    |Csc of string list
    |Fsc of FscCall
    |Other of string
    
let mapFsc (l:string) = 
    let whole = l
    let fscPath= l |> before "-o" |> trim
    let l = l |> after fscPath |> trim
    let l,targetObj = matchAndRemainder @"-o:([a-zA-Z0-9\\/.]+)" (fun m-> m.Groups.[1].Value) l
    let l,references = matchesAndRemainder  @"-r:""(.*?)""(\s|$)" (fun m -> m.Groups.[1].Value) l
    let l,assemblyAttrFile = matchAndRemainder @"""(.*?AssemblyAttributes.fs)""" (fun m-> m.Groups.[1].Value) l
    let l,files = matchesAndRemainder @"\w+.fs" (fun m -> m.Value) l
    let files = [files|> List.ofSeq; [assemblyAttrFile]] |> List.concat
    //let references = Regex.Matches(l, @"-r:""(.*?)""(\s|$)") |> Seq.cast<Match> |> Seq.map (fun m -> m.Groups.[1].Value) |> List.ofSeq
    Fsc {FscPath=fscPath;TargetObject=targetObj; References=references |> List.ofSeq; Files = files |> List.ofSeq;Remainder=l; (* Whole=whole *) }
    //l |> after "fsc.exe " |> split ' '|> Seq.skip 1 |> List.ofSeq |> Fsc
    
let mapCsc (l:string) = 
    l |> split ' ' |> List.ofSeq |> Csc
    
type MsBuildLine = {Line:int; Msg:BuildMessage}

let msbuildLinesTransformer (lines:string seq) = 
    lines //TODO: csc and fsc only make it through if they contain the word error =( map might need to come before filter
    |> Seq.mapi (fun i l -> i,l)
    |> Seq.filter (fun (_,l) -> l.Contains("error") || l.Contains("Error"))
    |> Seq.filter(fun (_,l) -> l.StartsWith("Task \"Error\" skipped") = false)
    |> Seq.filter(fun (_,l) -> Regex.IsMatch(l,@"^(Target|Task) ""\w+"" skipped") = false)
    |> Seq.map (fun (i,l) ->
                        {Line = i; Msg = 
                            if Regex.IsMatch(l,@"\\fsc.exe") then
                                mapFsc l
                            elif Regex.IsMatch(l,@"\\Csc.exe") then
                                mapCsc l
                            else Other l
                        })
let walkBuildAgents buildAgentPath projectCollectionName buildName f= 
    let buildName = match buildName with |Some buildName -> buildName | None -> null
    buildAgentPath
    |> Directory.GetDirectories
    |> Seq.map (fun agentDir -> Path.Combine(agentDir,projectCollectionName,buildName))
    |> Seq.filter Directory.Exists
    |> Dump
    |> Seq.map (fun buildDir -> buildDir.Dump("searchingBuildDir"); Directory.GetFiles(buildDir, "*.log", SearchOption.AllDirectories))
    |> Seq.filter (fun dirs -> dirs |> Seq.exists (fun _ -> true))
    |> f
    |> Dump
    
match buildStrategy with
|Latest ->
    walkBuildAgents buildAgentDir interestedProject None id
    |> Seq.map (fun logFilePaths -> logFilePaths |> Seq.map(fun logFilePath-> logFilePath,tryReadLines logFilePath))
    |> Dump
    |> ignore
    
|All buildName ->
    walkBuildAgents buildAgentDir interestedProject (Some buildName) id
    |> Seq.map (fun logFilePaths -> logFilePaths |> Seq.map(fun logFilePath-> logFilePath,tryReadLines logFilePath))
    |> Dump
    |> ignore
    
|LatestBuild buildName -> 
    walkBuildAgents buildAgentDir interestedProject (Some buildName) (fun f-> f |> Seq.maxBy( fun logFiles -> logFiles |> Dump |> Seq.map (fun l -> FileInfo(l).LastWriteTime) |> Seq.max))
    |> Dump
    |> Seq.map (fun logFilePath -> logFilePath,tryReadLines logFilePath)
    |> Seq.map (fun (f,lines) -> (My.PathWrapper(f).AsExplorerSelectLink(f)), bind' msbuildLinesTransformer lines)
    |> Dump
    |> ignore