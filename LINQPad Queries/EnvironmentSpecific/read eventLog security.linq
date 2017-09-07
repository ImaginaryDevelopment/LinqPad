<Query Kind="FSharpProgram" />

let myLog = new EventLog(Log="Security")
type LogonType = 
    |Logon of logonType:int
    |Exp of accountName:string*accountDomain:string
    with member private x.ToDump() = sprintf "%A" x
type LogonMap = { AccountName: string; AccountDomain: string; LogonType:LogonType; (* EventId is deprecated in favor of InstanceId *) InstanceId:int64; TimeGenerated:DateTime; Raw: obj}

let rMatch pattern opts x = 
    Regex.Match(x, pattern, options=opts)
    |> fun m -> 
        if m.Success then 
            Some m 
        else None
let (|RegexMatch|_|) (pattern:string) (x:string) = 
    rMatch pattern RegexOptions.None x
let (|RegexMultiMatch|_|) pattern x =
    rMatch pattern RegexOptions.Multiline x
    
let (|RegexMultiExpressions|_|) patterns message = 
    let ms = 
        // forward only, pattern order matters
        let mutable msg = message
        patterns
        |> Seq.map (fun (p,f) -> rMatch p RegexOptions.Multiline msg |> Option.map(fun m -> 
                let result = f m
                
                msg <- msg.[m.Index + m.Length ..]
                result
            )
        )
        |> List.ofSeq
    if ms |> Seq.exists (Option.isNone) then
        None
    else 
        let result = 
            ms 
            |> Seq.choose id
        result
        |> Some
let mapString (s:string) = 
    s |> Seq.map (fun l -> l, int l)
let ignoreBlacklistF = [
    (fun (e:EventLogEntry) -> e.EntryType = EventLogEntryType.FailureAudit)
]
let blacklistF = [
    (fun (e:EventLogEntry) -> e.Message = "A security-enabled local group membership was enumerated.")
]
type LogMessageType = 
    | Logon of LogonMap
    | Other of EventLogEntry
let transformPatterns =
    List.map (function 
        | title, Some(name,f) -> (sprintf "^\s*%s:\s+(?<%s>.+)$" title name, f name)  
        | pattern, None -> (pattern, fun _ x -> x)
    )
let sharedPatterns = 
    [
        "Subject:", None
        "Account\s+Name", Some("accountName", (fun (name:string) (m:Match) (lm:LogonMap) -> {lm with AccountName=m.Groups.[name].Value |> trim}))
        "Account\s+Domain", Some("accountDomain", (fun name m lm -> {lm with AccountDomain= m.Groups.[name].Value |> trim}))
    ]
let logonPatterns = 
    [
        "Logon\s+Type", Some("logonType", (fun (name:string) (m:Match) (lm:LogonMap) -> {lm with LogonType= m.Groups.[name].Value |> trim |> int |> LogonType.Logon }))
        
    ]
    |> (@) sharedPatterns
    |> transformPatterns
let expPatterns =
    [
        "Account\s+Whose\s+Credentials", None
        "Account\s+Name", Some("accountName", (fun (name:string) (m:Match) (lm:LogonMap) -> {lm with LogonType = LogonType.Exp(m.Groups.[name].Value |> trim, null)}))
        "Account\s+Domain", 
        Some("accountDomain", 
            (fun name m lm -> {lm with LogonType= 
                                        let value = m.Groups.[name].Value |> trim
                                        match lm.LogonType with 
                                        | LogonType.Exp(expN,_) -> LogonType.Exp(expN,value)
                                        | _ -> LogonType.Exp(null, value)
                                        }))
    ]
    |> (@) sharedPatterns
    |> fun x -> 
        x |> Seq.map fst |> Dump |> ignore
        x
    // add account name and domain again, changing the f
    |> transformPatterns
    
    

// consider showing top 100 focused results followed by random others (so we can spot interesting unfocused data
let partiallyMapped = 
    myLog.Entries
    |> Seq.cast<EventLogEntry>
    |> Seq.filter(fun x -> [ 4672; 4798;4799] |> Seq.map int64 |> Seq.exists ((=) x.InstanceId) |> not)
    |> Seq.filter(fun e -> 
        blacklistF |> Seq.exists(fun f -> f e) |> not  
        || ignoreBlacklistF |> Seq.exists(fun f -> f e)
        )
    
    |> Seq.map (fun e ->
        match e.Message with
        // """\s+Account Name:\s+(?<accountName>.+)$.*\s+Account Domain:\s+(?<accountDomain>.+)$(?:.*$)*\s+Logon\sID.*$(?:.*$)*\s+Logon\s"""
        
        | RegexMultiExpressions logonPatterns fs ->
            let empty = {AccountName=null; AccountDomain=null; InstanceId=e.InstanceId; Raw= box (Util.OnDemand("Raw", fun () -> e)); LogonType = LogonType.Logon 0; TimeGenerated= e.TimeGenerated}
            let result = 
                fs
                |> Seq.fold(fun x f -> f x) empty
            result
            |> Logon
        | RegexMultiExpressions expPatterns fs ->
            let empty = {AccountName=null; AccountDomain=null; InstanceId=e.InstanceId; Raw= box (Util.OnDemand("Raw", fun () -> e)); LogonType = LogonType.Exp(null,null) ; TimeGenerated= e.TimeGenerated}
            let result = 
                fs
                |> Seq.fold(fun x f -> f x) empty
            result
            |> Logon
        | _ -> Other e
    )

partiallyMapped
|> Seq.choose (function | Logon x -> Some x |_ -> None )
|> Seq.filter((fun x ->  x.AccountDomain = "WORKGROUP" && x.AccountName = "DESKTOP-AT08NJP$") >> not)
|> Dump
|> ignore

partiallyMapped
|> Seq.choose(function | Logon _ -> None | Other x -> Some x)
|> Seq.filter(fun x -> x.InstanceId >= 4800L || x.InstanceId <= 4801L)
|> Dump
|> ignore