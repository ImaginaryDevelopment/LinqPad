<Query Kind="FSharpProgram">
  <Namespace>System.Net</Namespace>
</Query>

type String with
	member self.Before(delimiter:string) =
			self.Substring(0,self.IndexOf(delimiter))
	member self.After(delimiter:string) =
			self.Substring(self.IndexOf(delimiter)+delimiter.Length)
type Response =
	| Success of response:string
	| Error of ex:WebException * error:string
	| AspException of references:seq<string> * ex:WebException
let GetResponse (site:string)=
	use wc =new WebClient()
	try        
		let output = wc.DownloadString(site)
		output.Dump("no error")
		output
	with | :? WebException as webEx -> 
		use stream=webEx.Response.GetResponseStream()
		use sr =new StreamReader(stream)
		let response=sr.ReadToEnd()
		match response with
			| x when x.Contains("id=\"compilerOutputDiv\"") -> 
				let decoded= WebUtility.HtmlDecode(response.After("id=\"compilerOutputDiv\"").After("<code>").Before("</code>"))
				let split = decoded.Before("Microsoft (R) Visual C#").After("<pre>").Trim().Split([|"/R:"|], StringSplitOptions.None).ToArray()
				split.Skip(1).Select(fun x -> x.Trim('"')).Dump(split.First())
				decoded
			| _ -> 
				response.Dump("no compilerOutputDiv")
				response
let site = 	match Util.ReadLine("site?","http://google.com") with
				| site when site.StartsWith("//") -> "http:"+site
				| site when site.Contains("://") -> site
				| site -> "http://"+site
site.Dump("checking site as")
GetResponse(site) |> ignore