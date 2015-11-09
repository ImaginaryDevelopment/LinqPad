<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Framework.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Tasks.v4.0.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Microsoft.Build.Utilities.v4.0.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.ComponentModel.DataAnnotations.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Configuration.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Design.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.Protocols.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.EnterpriseServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.Caching.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Security.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.ServiceProcess.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.ApplicationServices.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.RegularExpressions.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.Services.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>System.Net</Namespace>
  <Namespace>System.Web</Namespace>
</Query>

// compiles and runs
#if INTERACTIVE
open System
open System.Diagnostics
open System.Net
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Linq
open System.Web

type StringBuilder = System.Text.StringBuilder

type System.Object with
    member x.Dump(s) = 
        printfn "%s: %A" s x
    member x.Dump() = 
        printfn "%A" x
#endif

let delimit delimiter (items:#seq<string>) = String.Join(delimiter,items)
let indexOf (delimiter:string) (s:string) = s.IndexOf(delimiter)
let before delimiter s = (0,indexOf delimiter s) |> s.Substring
let after delimiter s = indexOf delimiter s + delimiter.Length |> s.Substring
let isMatch pattern s= Regex.IsMatch(s, pattern)
let isMatchI pattern s = Regex.IsMatch(s,pattern, RegexOptions.IgnoreCase)

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
        let attributesText =if attributes=null then String.Empty else attributes.Select( fun x -> x.Key+"='"+HttpUtility.HtmlAttributeEncode(x.Value)+"'") |> delimit " "
        sprintf "<%s %s%s" tag attributesText closing
    //public static string Include(string tag, IDictionary<string,string> attributes, string content){
    static member Include(tag:string, attributes:IDictionary<string,string>, content:string) =
        let closing="</"+tag+">"
        
        let attributesText = match attributes with
                                | null ->String.Empty 
                                | _ -> " "+ (attributes.Select(fun (x:KeyValuePair<string,string>) -> x.Key+"='"+HttpUtility.HtmlAttributeEncode(x.Value)+"'") |> delimit " ")
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
let Htmlify (title,lineCount,content) :string =
    let headContent = (sprintf "<title>%s</title>%s%s%s%s" 
                                        (WebUtility.HtmlEncode(title))
                                        Environment.NewLine
                                        (MyTagBuilder.IncludeCss("http://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"))
                                        Environment.NewLine
                                        (MyTagBuilder.IncludeInlineStyleSheet(".warn{ border-bottom:1px solid yellow;}"))
                        )
    let bodyContent = sprintf "<h2>%s %i  lines </h2>%s%s%s" (System.Net.WebUtility.HtmlEncode(title)) lineCount content Environment.NewLine (MyTagBuilder.IncludeScript("http://netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"))
    sprintf "<!DOCTYPE html>%s<html><head>%s</head>%s<body>%s%s%s</body></html>" Environment.NewLine headContent Environment.NewLine Environment.NewLine bodyContent Environment.NewLine

// Define other methods and classes here
//public string ReadBlock (IEnumerator<string> lines,string buildServer,string parentLine, string prevIndent){
let mutable index = 0

let Shallower(sb:StringBuilder,currentOpener:string,currentCloser:string) =
    Debug.Assert(sb.Length>=currentOpener.Length)
    if sb.Length<=currentOpener.Length then 
        true
    else 
        sb.AppendLine(currentCloser) |> ignore
        false
        
let Deeper(sb:StringBuilder) lines (prevIndent:string) (line:string) (readBlock:_ -> string) buildServer : unit =
    let indentLevel = if prevIndent = String.Empty then Regex.Match(line,@"\s+").Value else Regex.Match(line |> after prevIndent,@"\s+").Value // get the whitespace after the current indent level if there is one
    sb.AppendLine(System.Net.WebUtility.HtmlEncode(line.Trim())) |> ignore
    readBlock(lines,buildServer,line.Trim(), indentLevel) // readblock adds a UL and /UL
    |> sb.Append
    |> ignore
 
let MarkupFileReferences line server :string = 
    let fileReferences = _fileReferenceRegex.Matches(line);
    if fileReferences.Count = 0 then
        System.Net.WebUtility.HtmlEncode(line)
    else
        let sb = new StringBuilder()
    
        let mutable index=0
        for fr in fileReferences do
            if index<fr.Index then
                sb.Append(System.Net.WebUtility.HtmlEncode( line |> before fr.Value)) |> ignore
                index <- fr.Index
            let cleanedPath = System.IO.Path.GetFullPath(fr.Value) // eliminate c:\abc\..\def type stuff
        //firefox doesn't like this http://kb.mozillazine.org/Links_to_local_pages_don%27t_work
            let uncLinkStyle ="file://"+ (if firefoxUnc then "///" else String.Empty)
        
            let linkPath = 
                if System.IO.Directory.Exists(cleanedPath) || System.IO.File.Exists(cleanedPath) then //let link point locally if file exists locally, otherwise point out to server filesystem
                    cleanedPath 
                else 
                    (server+"\\"+cleanedPath.Replace(":\\","$\\"))
            sb.Append("<a href='" + uncLinkStyle
                + HttpUtility.HtmlAttributeEncode(linkPath.Replace('\\','/')) + "'" 
                + " title='"+HttpUtility.HtmlAttributeEncode(fr.Value) + "'>"
                + System.Net.WebUtility.HtmlEncode(linkPath)
                + "</a>") |> ignore
            index <- index + fr.Length

        if index<line.Length then
            sb.AppendLine(System.Net.WebUtility.HtmlEncode(line.Substring(index))) |> ignore
        else
            sb.AppendLine() |> ignore
        sb.ToString()

let MarkupLine line server = 
    //collect every mention of a .targets file then distinct by full path?
    MarkupFileReferences line server
        
let rec ReadBlock (lines:IEnumerator<string>,buildServer,parentLine:string,prevIndent) :string=
    let sb = StringBuilder();
    let openlist= "<ul title='line " + index.ToString() + "'>"
    let closelist =" </ul>"
    let (currentOpener:string,currentCloser:string) = 
        match parentLine with
        | null -> (openlist,closelist)
        | x when x.StartsWith("ForceNugetRestore:") -> 
            let cls = "forceNugetRestore"
            MyTagBuilder.BootstrapToggleButton("."+cls,"Collapse all "+cls)+"<div class='collapse "+cls+"'>"+openlist, "</div>"+closelist
        | x when x.StartsWith("_CopyFilesMarkedCopyLocal:") ->
            let cls = "_CopyFilesMarkedCopyLocal:"
            MyTagBuilder.BootstrapToggleButton("."+cls,"Collapse all "+cls)+"<div class='collapse "+cls+"'>"+openlist, "</div>"+closelist
        | _ -> (openlist,closelist)
    
    
    sb.Append(currentOpener) |> ignore
    let mutable doContinue = true
    
    while doContinue && lines.MoveNext() do
        index <- index+1
        let l:string = lines.Current
        let shallower = prevIndent <> String.Empty && l.StartsWith(prevIndent)=false
        let deeper = not shallower && l |> after prevIndent |> isMatch @"^\s"
        let addCurrentLine () =
            if l.Trim().Length=0 then () else
                    let classes = List<string>();
                    let glyphs = List<string>();
                    if l |> isMatchI "\\): warning " then 
                        glyphs.Add("glyphicon-warning-sign");
                        classes.Add("warn");
                    if l |> isMatchI "Copying" then classes.Add("copy")
                    let lineClass= 
                        match classes with
                            | c when c.Any()=true -> sprintf " class='%s'" (classes |> delimit " ")
                            | _ -> String.Empty
                    let glyphText =if glyphs.Any() then glyphs.Select(fun g -> "<span class='glyphicon "+g+"'></span>") |> delimit String.Empty else String.Empty
                    let reassembledLine = MarkupLine l buildServer
                    sb.AppendLine("<li" + lineClass + ">" + glyphText + reassembledLine + "</li>") |> ignore
        match deeper,shallower with
            | (false,false) -> 
                addCurrentLine()
            | (false,true) -> 
                addCurrentLine()
                let makeEmpty = Shallower(sb,currentOpener,currentCloser) //sb:StringBuilder,currentOpener:string,currentCloser:string
                if makeEmpty then sb.Clear() |> ignore
                doContinue <- false
            | (true,false) -> Deeper sb lines prevIndent l ReadBlock buildServer // buildServer lines ReadBlock buildServer
            | _ -> ()
        
        //new {indentLevel, l}.Dump("going deeper!");
        
    if doContinue then sb.Append(currentCloser) |> ignore
    sb.AppendLine() |> ignore
    sb.ToString()
    
//msbuild log file sectioning
let servers=
    Environment.GetEnvironmentVariable("servers").Dump()
    Environment.GetEnvironmentVariable("servers").Split(';')
    
let buildServer=Util.ReadLine("buildServer?",servers.FirstOrDefault(fun s -> s.Contains("build", StringComparison.CurrentCultureIgnoreCase)),servers)
    
let buildsPath = @"\\" + buildServer + "\\c$\\builds"
type BuildPath = {AgentFolder:string; TeamFolder:string; BuildFolder:string; BuildName:string}
let buildPaths = 
    query{
        for agentFolder in System.IO.Directory.EnumerateDirectories(buildsPath) do
        for teamFolder in System.IO.Directory.EnumerateDirectories(agentFolder) do
        for buildFolder in System.IO.Directory.EnumerateDirectories(teamFolder) do
        select {AgentFolder=agentFolder;TeamFolder=teamFolder;BuildFolder=buildFolder;BuildName=System.IO.Path.GetFileName(buildFolder)}
    }
    
let targetBuild : string =
    buildPaths.Select(fun bp -> bp.BuildName).ToArray().Dump()
    Util.ReadLine("target build?", null, buildPaths.Select(fun bp -> bp.BuildName).ToArray())
    
let buildDefinition = buildPaths.First(fun x -> x.BuildName.IsIgnoreCaseMatch(targetBuild))
let buildSrc = System.IO.Path.Combine( buildDefinition.BuildFolder,"Sources"); // our builds have the msbuild log files going into the src folder for the specific project

let mutable slnFolder = System.IO.Path.Combine(buildSrc, if targetBuild.Contains("PracticeManagement", StringComparison.InvariantCultureIgnoreCase) then "PM" else "Source")
if System.IO.Directory.Exists(slnFolder) = false && buildSrc.AsDirPath().GetFiles("*.sln").Any() then
    slnFolder <- buildSrc

Debug.Assert(System.IO.Directory.Exists(slnFolder),"Could not locate application slnFolder at "+slnFolder);

//var tempPath=System.IO.Path.GetTempPath();
type PInfo = {Id:int; ModuleName:string}
type LogInfo = {LogLink:obj; FileLink:obj; ExplorerLink:LINQPad.Hyperlinq;OpenLink:obj}
for logFile in System.IO.Directory.EnumerateFiles(slnFolder,"*.log", SearchOption.AllDirectories) do //consider caching previous log file locations and blacklisting debug and bin folders?
    let info = new System.IO.FileInfo(logFile)
    if info.Length=0L then
        info.Dump("Empty logfile"+logFile)
    else 
        printfn "Reading log file: %s" logFile
        let lines = System.IO.File.ReadLines(logFile)
        let mutable leaned:string = null
        use enumerator = lines.GetEnumerator()
        let content=ReadBlock(enumerator,buildServer,null,String.Empty)
        let cleaned = Htmlify(logFile,index,content)
        
        cleaned.SplitLines().Take(100).Dump("cleaned html")
        let tempFile = System.IO.Path.GetTempFileName()
        let targetFileName= tempFile+".htm"
        System.IO.File.Move(tempFile,targetFileName)
        System.IO.File.WriteAllText(targetFileName,cleaned)
        let targetFilePath=new My.PathWrapper(targetFileName)
        let mutable openLink:obj = null;
        if delayLoad then
            openLink <- Util.OnDemand("OpenFile",
                                fun () ->
                                    let p = Process.Start(targetFileName)
                                    {Id=p.Id; ModuleName=p.MainModule.ModuleName}
                )
                
        else
            let p = Process.Start(targetFileName)
            Debug.Assert(p <> null,"process was null")
            openLink <- {Id=p.Id;ModuleName = if p.MainModule <> null then p.MainModule.ModuleName else "null"} 
        
        let logFileWrapper= logFile.AsFilePath()
        {
            LogLink= logFileWrapper.ToAHref()
            FileLink = targetFilePath.ToAHref()
            ExplorerLink =targetFilePath.AsExplorerSelectLink("ExplorerLink")
            OpenLink= openLink
            }.Dump()
    ()
    
    
    //write out the file as html grouping based on indention?
