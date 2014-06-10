<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Configuration.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Framework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.ComponentModel.DataAnnotations.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.Caching.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.ApplicationServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.Services.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Utilities.v4.0.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Security.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.Protocols.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.EnterpriseServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Design.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Tasks.v4.0.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.ServiceProcess.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.RegularExpressions.dll</Reference>
  <Namespace>System.Web</Namespace>
</Query>

open System.Net
//does not compile, wip
let delayLoad=false
let firefoxUnc = true
let _fileReferenceRegex = new Regex(@"[a-e]:\\\w+(?:[a-z(0-9)\\.]|\s)+(?=\s|""|$|')", RegexOptions.IgnoreCase);

type MyTagBuilder =
	/// <summary>
	/// for self-closing links or links with no content
	/// </summary>
	//public static string Include(string tag, IDictionary<string,string> attributes, bool selfClose){
	static member Include(tag:string,attributes:IDictionary<string,string>,selfClose:bool) =
		let closing =if selfClose then " />" else "></"+tag+">"
		let attributesText =if attributes=null then String.Empty else attributes.Select( fun x -> x.Key+"='"+HttpUtility.HtmlAttributeEncode(x.Value)+"'").Delimit(" ")
		sprintf "<%s %s%s" tag attributesText closing
	//public static string Include(string tag, IDictionary<string,string> attributes, string content){
	static member Include(tag:string, attributes:IDictionary<string,string>, content:string) =
		let closing="</"+tag+">"
		
		let attributesText = match attributes with
								| null ->String.Empty 
								| _ -> " "+ attributes.Select(fun (x:KeyValuePair<string,string>) -> x.Key+"='"+HttpUtility.HtmlAttributeEncode(x.Value)+"'").Delimit(" ")
		attributesText.Dump("yay?")
		sprintf  "<%s%s>%s%s" tag attributesText content closing
	//public static string BootstrapToggleButton(string selector,string text){  //http://getbootstrap.com/javascript/#collapse
	static member BootstrapToggleButton (selector,text) =
		"<a data-toggle='collapse' data-target='"+selector+"'>"+HttpUtility.HtmlEncode(text)+"</a>"
	//public static string IncludeCss(string href){
	static member IncludeCss(href)=
		let coll = Dictionary<string,string>()
		["rel","stylesheet";"href",href] |> Seq.iter coll.Add
		MyTagBuilder.Include("link",coll,true)
	//public static string IncludeInlineStyleSheet(string content){
	static member IncludeInlineStyleSheet(content:string) =
		MyTagBuilder.Include("style",null,content)
	//public static string IncludeScript(string src){
	static member IncludeScript(src) =
		let coll = Dictionary<string,string>()
		["src",src] |> Seq.iter coll.Add
		MyTagBuilder.Include("script",coll,false)

//public static string Htmlify(string title, int lineCount,string content){
let Htmlify (title,lineCount,content) =
	let headContent = sprintf "<title>%s</title>%s%s%s%s" ,
						WebUtility.HtmlEncode(title), 
						Environment.NewLine, 
						MyTagBuilder.IncludeCss("http://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"),
						Environment.NewLine, 
						MyTagBuilder.IncludeInlineStyleSheet(".warn{ border-bottom:1px solid yellow;}")
	let bodyContent = sprintf "<h2>%s %i  lines </h2>%s%s%s", System.Net.WebUtility.HtmlEncode(title), lineCount, content,Environment.NewLine, MyTagBuilder.IncludeScript("http://netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js")
	sprintf "<!DOCTYPE html>%s<html><head>%s</head>%s<body>%s%s%s</body></html>", Environment.NewLine, headContent, Environment.NewLine, Environment.NewLine, bodyContent, Environment.NewLine

// Define other methods and classes here
//public string ReadBlock (IEnumerator<string> lines,string buildServer,string parentLine, string prevIndent){
let mutable index = 0;
let MarkupLine(l,buildServer) = 
	l
let Shallower(sb:StringBuilder,currentOpener:string,currentCloser:string) =
	Debug.Assert(sb.Length>=currentOpener.Length)
	if(sb.Length<=currentOpener.Length)
	then String.Empty
	else 
	//new{ line=l, prevIndent,prevIndentLength=prevIndent.Length,Chars= prevIndent.ToCharArray().Select(c=>(int)c),index}.Dump("going shallower");	
		sb.AppendLine(currentCloser) |> ignore
		sb.ToString()
let Deeper(sb:StringBuilder) =
	()
let Sibling(sb:StringBuilder,l:string,buildServer) =
	if l.Trim().Length=0 then () else
		let classes = List<string>();
		let glyphs = List<string>();
		if l.IsMatch("\\): warning ",true) then 
			glyphs.Add("glyphicon-warning-sign");
			classes.Add("warn");
		if l.IsMatch("Copying",true) then classes.Add("copy")
		
		let lineClass= 
			match classes with
				| c when c.Any()=true -> sprintf " class='%s'" (classes.Delimit(" "))
				| _ -> String.Empty
		let glyphText =if glyphs.Any() then glyphs.Select(fun g -> "<span class='glyphicon "+g+"'></span>").Delimit(String.Empty) else String.Empty
		let reassembledLine = MarkupLine(l,buildServer);
		sb.AppendLine("<li"+lineClass+">"+glyphText+reassembledLine+"</li>") |> ignore		
		
let rec ReadBlock (lines:IEnumerator<string>,buildServer,parentLine:string,prevIndent) =
	let sb = StringBuilder();
	let openlist= "<ul title='line "+index.ToString()+"'>";
	let closelist =" </ul>";
	let openClose = match parentLine with
						| null -> (openlist,closelist)
						| x when x.StartsWith("ForceNugetRestore:") -> 
							let cls = "forceNugetRestore"
							MyTagBuilder.BootstrapToggleButton("."+cls,"Collapse all "+cls)+"<div class='collapse "+cls+"'>"+openlist,		"</div>"+closelist
						| x when x.StartsWith("_CopyFilesMarkedCopyLocal:") ->
							let cls = "_CopyFilesMarkedCopyLocal:"
							MyTagBuilder.BootstrapToggleButton("."+cls,"Collapse all "+cls)+"<div class='collapse "+cls+"'>"+openlist,		"</div>"+closelist
						| _ -> (openlist,openlist)
	let (currentOpener:string,currentCloser:string) = openClose;
	
	sb.Append(currentOpener) |> ignore
	
	while lines.MoveNext() do
		index<= index+1 |> ignore
		let l = lines.Current
		let shallower = prevIndent<>String.Empty && l.StartsWith(prevIndent)=false
		let deeper = not shallower && l.After(prevIndent).IsMatch(@"^\s",false)
		match deeper,shallower with
			| (false,false) -> Sibling(sb,l,buildServer)
			| (false,true) -> Shallower(sb,currentOpener,currentCloser) //sb:StringBuilder,currentOpener:string,currentCloser:string
			| (true,false) -> do
				sb.Append(Deeper(sb,l,buildServer)) |> ignore
				let indentLevel =if prevIndent=string.Empty then Regex.Match(l,@"\s+").Value else  Regex.Match(l.After(prevIndent),@"\s+").Value // get the whitespace after the current indent level if there is one		
				sb.AppendLine(WebUtility.HtmlEncode(l.Trim()))
				sb.Append(ReadBlock(lines,buildServer,l.Trim(),indentLevel)) |> ignore
			| _ -> ()
		
		//new {indentLevel, l}.Dump("going deeper!");
		
	sb.Append(currentCloser)
	sb.ToString()

public static string MarkupLine(string line,string server){
	//collect every mention of a .targets file then distinct by full path?
	return MarkupFileReferences(line,server);
}

public static string MarkupFileReferences(string line,string server){
	var fileReferences = _fileReferenceRegex.Matches(line);
	if(fileReferences.Count==0)
		return System.Net.WebUtility.HtmlEncode(line);
	var sb= new StringBuilder();
	
	var index=0;
	foreach(Match fr in fileReferences){
		if(index<fr.Index){
			sb.Append(System.Net.WebUtility.HtmlEncode( line.Before(fr.Value)));
			index=fr.Index;
		}
		var cleanedPath = System.IO.Path.GetFullPath(fr.Value); // eliminate c:\abc\..\def type stuff
		//firefox doesn't like this http://kb.mozillazine.org/Links_to_local_pages_don%27t_work
		var uncLinkStyle ="file://"+ (firefoxUnc? "///":string.Empty);
		
		var linkPath = System.IO.Directory.Exists(cleanedPath) || System.IO.File.Exists(cleanedPath) ? //let link point locally if file exists locally, otherwise point out to server filesystem
			cleanedPath: (server+"\\"+cleanedPath.Replace(":\\","$\\"));
		sb.Append("<a href='"+uncLinkStyle
			+HttpUtility.HtmlAttributeEncode(linkPath.Replace('\\','/')) +"'" 
			+" title='"+HttpUtility.HtmlAttributeEncode(fr.Value)+"'>"
			+System.Net.WebUtility.HtmlEncode(linkPath)
			+"</a>");
		index+=fr.Length;
	}
	if(index<line.Length){
		sb.AppendLine(System.Net.WebUtility.HtmlEncode(line.Substring(index)));
	} else {
		sb.AppendLine();
	}
	return sb.ToString();
}


void Main()
{
	
	//msbuild log file sectioning
	var servers=Environment.GetEnvironmentVariable("servers").Dump().Split(';');
	var buildServer=Util.ReadLine("buildServer?",servers.FirstOrDefault(s=>s.Contains("build", StringComparison.CurrentCultureIgnoreCase)),servers);
		
	var buildsPath = @"\\"+buildServer+"\\c$\\builds";
	var buildPaths = from agentFolder in System.IO.Directory.EnumerateDirectories(buildsPath)
					from teamFolder in System.IO.Directory.EnumerateDirectories(agentFolder)
					from buildFolder in System.IO.Directory.EnumerateDirectories(teamFolder) 
				select new{agentFolder,teamFolder,buildFolder,BuildName=System.IO.Path.GetFileName(buildFolder)};
	var targetBuild=Util.ReadLine("target build?", null, buildPaths.Select(bp=>bp.BuildName).ToArray().Dump()); 
	var buildDefinition =buildPaths.First(x=>x.BuildName.IsIgnoreCaseMatch(targetBuild));
	var buildSrc = System.IO.Path.Combine( buildDefinition.buildFolder,"src"); // our builds have the msbuild log files going into the src folder for the specific project
	
	var productFolder = System.IO.Path.Combine(buildSrc,"Products");
	
	var slnFolder = System.IO.Path.Combine(productFolder,targetBuild.Contains("CV", StringComparison.InvariantCultureIgnoreCase)?"CVS":"MarketOnce");
	if(!System.IO.Directory.Exists(productFolder) && buildSrc.AsDirPath().GetFiles("*.sln").Any()){
		productFolder=null;
		slnFolder=buildSrc;
	}
	
	Debug.Assert(System.IO.Directory.Exists(slnFolder),"Could not locate application slnFolder at "+slnFolder);
	
	//var tempPath=System.IO.Path.GetTempPath();
	
	foreach(var logFile in System.IO.Directory.EnumerateFiles(slnFolder,"*.log", SearchOption.AllDirectories)){ //consider caching previous log file locations and blacklisting debug and bin folders?
		var info = new System.IO.FileInfo(logFile);
		if(info.Length==0){
			info.Dump("Empty logfile"+logFile);
		}
		var lines = System.IO.File.ReadLines(logFile) as IEnumerable<string>;
		string cleaned;
		using(IEnumerator<string> enumerator = lines.GetEnumerator()){
		var content=ReadBlock(enumerator,buildServer,null,string.Empty);
			cleaned=Htmlify(logFile,index,content);
		}
		
		cleaned.SplitLines().Take(100).Dump("cleaned html");
		var tempFile = System.IO.Path.GetTempFileName();
		var targetFileName= tempFile+".htm";
		System.IO.File.Move(tempFile,targetFileName);
		System.IO.File.WriteAllText(targetFileName,cleaned);
		var targetFilePath=new My.PathWrapper(targetFileName);
		object openLink;
		if(delayLoad){
		 openLink = Util.OnDemand("OpenFile",
			()=>{
				var p =Process.Start(targetFileName);
				return new{p.Id, p.MainModule.ModuleName}; 
				});
				
		} else {
			var p = Process.Start(targetFileName);
			Debug.Assert(p!=null,"process was null");
			openLink= new{p.Id,ModuleName = p.MainModule!=null? p.MainModule.ModuleName:"null"} ;
		}
		
		var logFileWrapper= logFile.AsFilePath();
		new{LogLink= logFileWrapper.ToAHref() ,FileLink = targetFilePath.ToAHref(), ExplorerLink =targetFilePath.AsExplorerSelectLink("ExplorerLink"),
		OpenLink= openLink}.Dump();
		
		
		//write out the file as html grouping based on indention?
	}
	
}