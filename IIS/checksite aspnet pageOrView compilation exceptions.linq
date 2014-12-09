<Query Kind="FSharpProgram">
  <Namespace>System.Net</Namespace>
</Query>

//type String with
//	member self.Before(delimiter:string) =
//		self.Substring(0,self.IndexOf(delimiter))
//	member self.After(delimiter:string) =
//		self.Substring(self.IndexOf(delimiter)+delimiter.Length)
let indexOf (delimiter:string) (s:string) :int = 
	s.IndexOf delimiter
indexOf "test" "hellotest" |> Dump
let lastIndexOf (delimiter:string) (s:string) :int =
	s.LastIndexOf delimiter

	
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
	
let xOrSelf func delimiter (s:string) = 
	if s.Contains delimiter then func s else s
let beforeOrSelf delimiter (s:string) = 
	if s.Contains(delimiter) then
		before delimiter s
	else s
	
let trim (s:string) = s.Trim()

let split (delimiters: string[]) (options:StringSplitOptions) (s:string) = 
	s.Split(delimiters,options)
	
type Response =
	| Success of response:string
	| Error of ex:WebException * error:string
	| AspException of references:seq<string> * ex:WebException
	
let (|TempAsp|_|) (path:string) = 
	let tempAspDelimiter = "\\Temporary ASP.NET Files\\"
	if path.Contains tempAspDelimiter then
		let rest = after tempAspDelimiter path
		Some <| (before tempAspDelimiter path, tempAspDelimiter, beforeLast "\\" rest,afterLast "\\" rest |> before "\"")
	else None
	
let parse (referencePath:string) = 
	match referencePath with
	| TempAsp(start,delim,_,fileName) -> start,delim,fileName
	| _ -> referencePath |> beforeLast "\\" ,String.Empty, referencePath|> afterLast "\\"
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
				let decoded = WebUtility.HtmlDecode codeSection
				let compilationSource = WebUtility.HtmlDecode <| codeSections |> after "id=\"dynamicCodeDiv" |> after "<code><pre>"
				let fileName = compilationSource |> before "Line 2:" |> beforeOrSelf "\" \"{" |> after "checksum \""
				Some(decoded,fileName,compilationSource)
			else None
			
		match response with
			| CompilationErrorPage(compilerOutput,filename,compilationSource) -> 
				// decoded.Dump("decodedraw")
				let refs = 
					// assumes /out is not followed by any refs /R:
					compilerOutput 
					|> before "Microsoft (R) Visual C#"
					|> after "<pre>"
					|> before "/out:"
					|> trim 
					|> split [|"/R:";"/out:"|] StringSplitOptions.None
					|> Seq.sortBy id (* id is a reserved word or keyword *)
					|> Seq.skip 1 // csc.exe call
					|> Seq.map parse
				refs.Dump(filename)
			| _ -> 
				response.Dump("no compilerOutputDiv")
				
				
let site = 	match Util.ReadLine("site?","http://localhost:60617/") with
				| site when site.StartsWith("//") -> "http:"+site
				| site when site.Contains("://") -> site
				| site -> "http://"+site
site.Dump("checking site as")
GetResponse(site) |> ignore