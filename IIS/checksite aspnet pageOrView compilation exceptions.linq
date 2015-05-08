<Query Kind="FSharpProgram">
  <Namespace>System.Net</Namespace>
</Query>


#region "string helpers"
let indexOf (delimiter:string) (s:string) :int = 
	s.IndexOf delimiter
let lastIndexOf (delimiter:string) (s:string) :int =
	s.LastIndexOf delimiter
let trim (s:string) = s.Trim()
let split (delimiters: string[]) (options:StringSplitOptions) (s:string) = 
	s.Split(delimiters,options)

let private b4 f (s:string) = 
	s.Substring(0,f s)
let private aft f (s:string) = 
	s.Substring(f s)
	
let before (delimiter:string) (s:string) :string =
	b4 <| indexOf delimiter <| s
	
let after (delimiter:string) (s:string) :string = 
	indexOf delimiter >> (+) delimiter.Length
	|> aft <| s

let beforeLast (delimiter:string) (s:string) :string = 
	if s.Contains delimiter then 
		b4 <| lastIndexOf delimiter <| s
	else s
	
let afterLast delimiter (s:string) = 
	lastIndexOf delimiter >> (+) delimiter.Length
	|> aft <| s
	
let tests = 
	if before "world" "hello world" |> (<>) "hello " then failwithf "before is wrong"
	let test (actual:'a) op (expected:'a) = 
		if actual |> op expected then failwithf "expected %A, but was %A" expected actual
	test <| before "world" "hello world" <| (<>) <| "hello "
	test <| after "hello" "hello world" <| (<>) <| " world"
	if after "hello" "hello world" |> (<>) " world" then failwithf "after is wrong"
	if beforeLast "world" "helloworldworld" |> (<>) "helloworld" then failwithf "beforeLast is wrong"
	test <| afterLast "hello" "helloworldhelloworld" <| (<>) <| "world"
	
//let xOrSelf func delimiter (s:string) = 
//	if s.Contains delimiter then func s else s

let beforeOrSelf delimiter (s:string) = 
	if s.Contains(delimiter) then
		before delimiter s
	else s
#endregion	"string helpers"

type Response =
	| Success of response:string
	| Error of ex:WebException * error:string
	| AspException of references:seq<string> * ex:WebException

let (|Gac|_|) (path:string) : (string*string*string*string) option = 
	let gacDelimiter = "\\GAC_"
	if path.Contains gacDelimiter then
		let afterDelimiter:string = path |> after gacDelimiter |> after "\\"
		let delimiter:string = path |> after gacDelimiter |> before "\\" |> (+) gacDelimiter
		Some <| (before gacDelimiter path, delimiter,afterDelimiter |> beforeLast "\\", afterDelimiter |> afterLast "\\"  |> before "\"")
	else None
let appData = Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData
(appData,Environment.ExpandEnvironmentVariables("%localappdata%")).Dump("appData")
let (|TempAsp|_|) (path:string) = 
	let tempAspDelimiter = "\\Temporary ASP.NET Files\\"
	if path.Contains tempAspDelimiter then
		let rest = after tempAspDelimiter path
		let start = if path.StartsWith appData then "%localappdata%" + (path |> before tempAspDelimiter |> after appData ) else path |> before tempAspDelimiter
		Some <| (start, tempAspDelimiter, beforeLast "\\" rest,afterLast "\\" rest |> before "\"")
	else None
	
let parse (referencePath:string) = 
	match referencePath with
	| TempAsp(start,delim,_,filename) -> start,delim,filename
	| Gac(start, delim, _, filename) -> start,delim,filename
	| _ -> referencePath |> beforeLast "\\" ,String.Empty, referencePath|> afterLast "\\" |> before "\""
	
type WebReply = 
	| Response of string
	| ErrResponse of string * int * WebException // can we get the status code?
	
let getResponse (site:string) = 
	use wc =new WebClient()
	try        
		let output = wc.DownloadString(site)
		Response (output)
	with | :? WebException as webEx -> 
		use stream=webEx.Response.GetResponseStream()
		use sr =new StreamReader(stream)
		
		let response=sr.ReadToEnd()
		//failwithf "error!"
		ErrResponse(response,int((webEx.Response :?> HttpWebResponse).StatusCode), webEx)
		
let GetResponse (site:string)=
	match getResponse site with
	| Response output -> output.Dump("no error")
	| ErrResponse(response,statusCode, webEx) -> 
		let (|CompilationErrorPage|_|) (input:string) = 
			if input.Contains("id=\"compilerOutputDiv\"") then
				let codeSections = response |> after "id=\"compilerOutputDiv\"" |> after "<code>"
				let codeSection = codeSections |> before "</code>"
				let compilerOutput = WebUtility.HtmlDecode codeSection
				let compilationSource = WebUtility.HtmlDecode <| codeSections |> after "id=\"dynamicCodeDiv" |> after "<code><pre>"
				let fileName = compilationSource |> before "Line 2:" |> beforeOrSelf "\" \"{" |> after "checksum \""
				Some(compilerOutput,fileName,compilationSource)
			else None
			
		match response with
			| CompilationErrorPage(compilerOutput,filename,compilationSource) -> 
				let refs = 
					// assumes /out is not followed by any refs (/R:)
					compilerOutput 
					|> before "Microsoft (R) Visual C#"
					|> after "<pre>"
					|> before "/out:"
					|> trim 
					|> split [|"/R:\"";"/out:"|] StringSplitOptions.None
					|> Seq.skip 1 // csc.exe call
					//|> Seq.sortBy id (* id is a reserved word or keyword *)
					|> Seq.map parse
					|> Seq.sortBy (fun (_,_,filename) -> filename)
				refs.Dump(filename)
			| _ -> 
				response.Dump("no compilerOutputDiv")
				
				
let site = 	match Util.ReadLine("site?","http://localhost:60617/") with
				| site when site.StartsWith("//") -> "http:"+site
				| site when site.Contains("://") -> site
				| site -> "http://"+site
site.Dump("checking site as")
GetResponse(site) |> ignore