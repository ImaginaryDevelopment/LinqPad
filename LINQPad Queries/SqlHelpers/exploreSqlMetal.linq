<Query Kind="FSharpProgram" />

// explore sqlmetal
let interestedItem = "UspPaymentsGet"
let interestedType = "Function" // Table or Function

let delimit (d:string) (items: string seq) = String.Join(d, items)
let(|ValueString|NonValueString|) = 
    function
    | "" | null -> NonValueString
    | x when String.IsNullOrWhiteSpace x -> NonValueString
    | x -> ValueString x
    

let dc = new TypedDataContext()
// buffer this, it gets wiped out at some point later
let cs = dc.Connection.ConnectionString
type OutputType =
    |E of string
    |O of string
let runIt cmd args = 
    let psi = ProcessStartInfo(cmd,args)
    psi.RedirectStandardError <- true
    psi.RedirectStandardOutput <- true
    psi.UseShellExecute <- false
    psi.RedirectStandardInput <- true
//    psi.CreateNoWindow <- true
    use p = new Process()
    p.EnableRaisingEvents <- true
    p.StartInfo <- psi
    let output = ResizeArray()
    let disp = p.ErrorDataReceived.Subscribe(fun ed -> output.Add(E ed.Data))
    let disp2 = p.OutputDataReceived.Subscribe(fun d -> output.Add(O d.Data))
    let ec = 
        try
            try
                p.Start() |> ignore<bool>
                p.BeginOutputReadLine()
                p.BeginErrorReadLine()
                p.WaitForExit()
                p.ExitCode |> Some
            with ex -> 
                ex.Dump()
                None
        finally
            disp.Dispose()
            disp2.Dispose()
    (ec,output :> _ seq)
    
module Option=
    let getOrDefault x = 
        function
        | Some x -> x
        | None -> x
    let ofValueString =
        function
        | "" | null -> None
        | x when String.IsNullOrWhiteSpace x -> None
        | x -> Some x
        
let getAttribValue name (x:XElement) : string =
    let xn = XName.op_Implicit name
    x.Attribute(xn)
    |> Option.ofObj
    |> Option.map(fun x -> x.Value)
    |> Option.getOrDefault null

// Define other methods and classes here
let getCommandOptions(commandPath:string) =
    try
        Util.Cmd(commandPath,"/?")
    with | :? CommandExecutionException as ex -> 
        ex.Dump()
        reraise()
    
let targetPath = Path.Combine( Path.GetTempPath(),"linqpad_sqlmetal")
Directory.CreateDirectory(targetPath) |> ignore
Environment.CurrentDirectory <- targetPath
Environment.CurrentDirectory.Dump("cwd")
let searchPath = Path.Combine(Environment.GetFolderPath( System.Environment.SpecialFolder.ProgramFilesX86), "Microsoft SDKs","Windows")
let sqlMetals = System.IO.Directory.EnumerateFiles(searchPath,"sqlmetal.exe", SearchOption.AllDirectories) |> Array.ofSeq
sqlMetals.Dump()
sqlMetals
|> Seq.iter(fun sqlMetal ->
    sqlMetal.Dump("selected")

    //let entireOutput = Util.Cmd(sqlMetal,"/conn:\""+ dc.Connection.ConnectionString + "\" /sprocs",quiet=true);
    
    let ec,text = 
        try
            runIt sqlMetal ("/conn:\""+ cs + "\" /sprocs")
        with ex -> ex.Dump("failed to run");(None,Seq.empty)
    let notXml (s:string) = not (isNull s) && s.StartsWith("<") = false
    let data = text |> Seq.map(function |E x -> x | O x -> x) |> Seq.choose(Option.ofValueString) |> List.ofSeq
    let xml = data |> Seq.skipWhile notXml |> List.ofSeq
    match ec, xml with
    | Some 0,ValueString _::_ -> None
    | Some x,_ -> Some()
    | None,_ -> printfn "exception?";None
    |> Option.iter(fun _ -> 
        (sqlMetal,ec, data).Dump()
    )
    
    xml
    |> delimit Environment.NewLine |> Option.ofValueString
    |> Option.iter(fun xml ->
        printfn "processing xml for %s" sqlMetal
        try
            match xml with
            | null | "" -> printfn "Exit code was %A" ec
            | xml ->
                try
                	let doc = XDocument.Parse(xml)
                	doc.DumpFormatted() |> ignore
                	match interestedType with
                    | "Function" ->
            		    let es = doc.Root.Elements(doc.Root.Name.Namespace + "Function")
            		    es
            			//.Dump("functions")
                        |> Seq.tryFind(fun r ->
            			    let a = getAttribValue "Name" r
                            a.EndsWith(interestedItem, StringComparison.InvariantCultureIgnoreCase)
                        )
                        |> Option.iter(fun (fElem:XElement) ->
                            fElem.Dump()
            			    let a:string= getAttribValue "Name" fElem
            				dc.sys.sp_helptext(a,null).Dump()
            				|> ignore
                	    )
                        ()
                    | _ -> ()
                with ex -> ex.Dump()
        finally 
        	data.Where(fun l -> l.Contains("Warning") || l.Contains(interestedItem)).OrderByDescending(fun l -> l.Contains(interestedItem)).Dump("interesting")
        	()
            
            Directory.EnumerateFiles(Environment.CurrentDirectory).Dump("files in target dir");
            data
                .SkipWhile(fun s-> s.StartsWith("<") = false)
                .SkipWhile(fun s -> s.Contains("<"))
                .Dump("after doc output")
        try
            getCommandOptions(sqlMetal).Dump("cmd options")
        with ex -> (sqlMetal,ex).Dump("could not get cmd options")
    )
)