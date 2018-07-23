<Query Kind="FSharpProgram" />

type ScQueryOutput = 
    {
        ServiceName:string 
        DisplayName:string 
        Type:string 
        State:string 
        Unmapped:string 
    }
module Seq =
    let groupLinesBy delimiter text =
        seq{
            let sb = StringBuilder()
            let empties = StringBuilder()
            for item in text |> Seq.skipWhile String.IsNullOrWhiteSpace do
                if item.StartsWith delimiter && sb.Length > 0 then
                    yield sb.ToString()
                    sb.Clear() |> ignore
                if String.IsNullOrWhiteSpace item then
                    empties.AppendLine item |> ignore
                else
                    sb.AppendLine item |> ignore
                    empties.Clear() |> ignore
            if sb.Length > 0 then
                yield sb.ToString()
        }
    let any = Seq.exists(fun _ -> true)
    let bufferByCount<'T> chunkSize values = 
        seq{
            let mutable total = 0
            let mutable current = values
            while (any current) do
                yield current.Take(chunkSize)
                total <- total + chunkSize
                current <- current.Skip(chunkSize)
        }

let transformScQuery (output:string) = 
//    output.Dump()
    let grouped = output.SplitLines().SkipWhile(String.IsNullOrEmpty) |> Seq.groupLinesBy "SERVICE_NAME"
    let lines = grouped |> Seq.map(fun g -> g.SplitLines() |> Seq.map (fun l -> l.AfterOrSelf(": ")) |> Array.ofSeq)
    seq{
        for line in lines do
            let serviceName = line.[0]
            yield 
                {
                    ServiceName = serviceName
                    DisplayName = line.[1]
                    State = line.[3] + line.[4]
                    Type = line.[2]
                    Unmapped = line.Skip(4).Delimit(Environment.NewLine)
                }
    }
let allValues<'T when 'T : struct>() =
    Enum.GetNames(typeof<'T>)
    |> Seq.map (fun v -> Enum.Parse(typeof<'T>,v) :?> 'T)
type System.String with
    member x.TruncateTo count = 
        match x with
        | null -> x
        | x when x.Length <= count -> x
        | x -> x.Substring(0,count)
type System.IO.StreamReader with
    member x.ReadToEndAndDispose() =
        use r = x
        r.ReadToEnd()


let start = "start"
let stop = "stop"
let query = "query"

let options = [start; stop; query]
let current = Util.ReadLine("Desired action?", query, options)
let servers = 
    System.Environment.GetEnvironmentVariable("servers", EnvironmentVariableTarget.User)
    |> function
        | ""
        | null -> [| "localhost" |]
        | x -> x.Split(';')
let server = Util.ReadLine("Server?", servers.[0], servers);

let psi = ProcessStartInfo("sc", 
        	RedirectStandardError = true,
        	RedirectStandardOutput = true, //,RedirectStandardInput=true 
        	UseShellExecute = false,
        	ErrorDialog = false,
        	CreateNoWindow = true
) // WinMgmt or WMSvc?

let mutable queryResult = null
let mutable startOutput = Unchecked.defaultof<MyExtensions.StreamOuts>
let mutable startHadError = false
let mutable toStart = null
type QrDisplay = {DisplayName:string; State:string;}
using (new Process()) ( fun ps ->
	ps.StartInfo <- psi

	//let input=ps.StandardInput;
	let queryOutputs = ps.RunProcessRedirected(@"\\" + server + " query state= all type= service")
	if queryOutputs.Errors.HasValue() then
        ()
    else
    	queryResult <- transformScQuery queryOutputs.Output
    	//queryResult.Dump("all");
    	let mutable validSvcs = queryResult
    	if current = start then
    		validSvcs <- validSvcs.Where(fun r -> r.State.StartsWith "4" = false)
    	else if current = stop then
    		validSvcs <- validSvcs.Where(fun r -> r.State.StartsWith "4")
    
    	if (validSvcs.Any() = false) then
    		Util.Highlight("No valid services found to " + current).Dump();
    		queryResult.Select(fun q -> { DisplayName=q.DisplayName; State = q.State.RemoveMultipleWhitespaces() }).Dump("Found services on " + server)
    	else
        	Util.HorizontalRun(false, validSvcs
        		.Select(fun r -> r.ServiceName + "(" + r.State.RemoveMultipleWhitespaces().TruncateTo(12) + ")") |> Seq.bufferByCount 18).Dump()
        	toStart <- Util.ReadLine(current + " which service?", "WMSvc", queryResult.Select(fun r -> r.ServiceName))
        
        	if toStart.HasValue() then
        		startOutput <- ps.RunProcessRedirected(@"\\" + server + " " + current + " " + toStart)
)
//ps disposed

if startOutput.Errors.HasValue() || startOutput.Output.Contains("FAILED 1060:") then
	startHadError <- true

Util.ClearResults()

Util.Highlight(startOutput.Output.Trim()).Dump("attempted to " + current + ":" + toStart)
if (toStart.HasValue() = false || startHadError) then
	queryResult.Dump("sc query result");

//	 Util.HorizontalRun(false, validSvcs
//	 	.Select (r =>r.ServiceName+"("+r.State.RemoveMultipleWhitespaces().TruncateTo(12)+")").BufferByCount(18)).Dump("services found");
