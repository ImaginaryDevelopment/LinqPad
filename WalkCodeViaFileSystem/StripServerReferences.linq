<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

//using html agility pack
//assumes all <script></script> tags with inline js have just been cut/pasted into a new js file with the same name as the page
//then searches that script for all the server tags, and transforms them into something javascript friendly
//then writes out the cleaned javascripts into the new file.
//and dumps the necessary javascript server-based initializers to the screen output

//<%= servercontrol.ClientID%>
//becomes servercontrol.ClientID in the .js file
void Main()
{
	
	var appPagePath=DetermineAppAndPagePath();
	var pagePath = System.IO.Path.Combine(appPagePath.Item1, appPagePath.Item2);
	if(!System.IO.File.Exists(pagePath))
	{
		("Could not find page:"+pagePath).Dump();
		return;
	}
	var scripts=LocateScriptTags(pagePath);
	
	var jsPath=LocateExternalScriptReferencePath(scripts,appPagePath.Item1, appPagePath.Item2);
	
	var oldScript = System.IO.File.ReadAllText(jsPath);
	if(Regex.IsMatch(oldScript,ServerTagPattern, RegexOptions.IgnorePatternWhitespace)==false)
	{
		"Nothing to transform found".Dump();
		return;
	}
	var newTags=TransformTags(Regex.Matches(oldScript,ServerTagPattern,RegexOptions.IgnorePatternWhitespace).Cast<Match>());
	newTags.Dump();
	var newScript=Regex.Replace(oldScript,ServerTagPattern,"$1.$2", RegexOptions.IgnorePatternWhitespace)
		.Dump("newscript");
	var hasErrorTags=newScript.Contains("<%") || newScript.Contains("<asp:");
	if(hasErrorTags)
		hasErrorTags.Dump("has error tags");
	if(newScript.Contains("@"))
	{
		"newScript has warning tags".Dump();
	}
	
	if(File.Exists(jsPath))
	{
		jsPath.Dump("destination already exists");
		return;
	}
	File.WriteAllText(jsPath,newScript);
	
	jsPath.Dump("Written to");
}

public IEnumerable<string> TransformTags(IEnumerable<Match> items)
{
	foreach(var item in items)
	{
		Debug.Assert(item.Groups[2].Value=="ClientID");
	}
	foreach(var m in items.Select(s=>s.Groups[1].Value).Distinct().OrderBy(s=>s))
	{
		//uwgDealRegions = { ClientID: "<%=uwgDealRegions.ClientID%>" };
		yield return m+"={ClientID: \"<%="+m+".ClientID%>\" };";	
	}
}

// Define other methods and classes here
public Tuple<string,string> DetermineAppAndPagePath()
{
	var defaultDrive=System.IO.Directory.Exists(@"D:\Development")? "D":"C";
	var baseDir=defaultDrive+@":\Development\";
	var defaultPage=@"Pages\Admin\EditOrg.aspx";
	var page=LINQPad.Util.ReadLine("Page location?",defaultPage); //
	return Tuple.Create(appPath,page);
}

public HtmlAgilityPack.HtmlNodeCollection LocateScriptTags(string pagePath)
{
	var pageHtml=new HtmlAgilityPack.HtmlDocument();
	pageHtml.OptionWriteEmptyNodes=true;
	//parser needs everything wrapped in one big tag
	pageHtml.LoadHtml("<asp>"+File.ReadAllText(pagePath)+"</asp>");
	if(pageHtml.ParseErrors.Any())
	pageHtml.ParseErrors.Dump(pagePath);
	pageHtml.DocumentNode.ChildNodes.First().ChildNodes.Count.Dump("node count");
	var innerPage=pageHtml.DocumentNode.ChildNodes.First();
	//innerPage.Name.Dump("Name");
	var scripts=innerPage.SelectNodes("//script");
	scripts.Count.Dump("Scripts found");
	
	//var scripts=pageStart.Where(p=>p.Name=="script");
	//if(scripts.Count()!=2)
	//throw new ArgumentOutOfRangeException("page","Page should already be set with a src= script and an empty script tag");
	return scripts;
}

public string LocateExternalScriptReferencePath(HtmlAgilityPack.HtmlNodeCollection scripts,string appPath, string pagePath)
{
	var fullPagePath = System.IO.Path.Combine(appPath,pagePath);
	var scriptsWithSrcTags = scripts.Where(s=>s.HasAttributes && s.Attributes.AttributesWithName("src").Any()).Select(s=>s.Attributes).Dump("scripts");
	var srcLocator=scripts.First(s=>s.HasAttributes && s.Attributes.AttributesWithName("src").Any() && !s.Attributes.AttributesWithName("src").First().Value.StartsWith("//")).Attributes["src"].Value;
	srcLocator.Dump("jsSource");
	
	//var insertLocation=scripts.FirstOrDefault(s=>s.Attributes.AttributesWithName("src").Any()==false);
	Path.GetDirectoryName(fullPagePath).Dump("full page path");
	srcLocator.Replace('/','\\').Dump("jsSrcLocator");
	
	var srcPath=
		(srcLocator.StartsWith("~")?
			System.IO.Path.Combine(appPath,srcLocator.After("~/").Replace('/','\\')) :
		System.IO.Path.Combine(Path.GetDirectoryName(pagePath),srcLocator.Replace('/','\\'))).Dump("jsSrcPath");
	return srcPath;
}

const string ServerTagPattern=@"[\'""]? <% \s* = \s* ([\w_]*) \.? ([\w.]*) \s* %> \s* [\'""]?";

public IEnumerable<string> TransformControlCalls(string pageText)
{
	
	var regex=Regex.Matches(pageText,ServerTagPattern, RegexOptions.IgnorePatternWhitespace);
	var outputLines=new List<string>();
	foreach(var captureGroup in regex.Cast<Match>().Select(m=>m.Groups).GroupBy(m=>m[1].Value+'.'+m[2].Value).Select(g=>g.First()))
	{
		var control=captureGroup[1].Value;
		var subCall=captureGroup[2].Value;
		var outputLine=String.Format("{0} = {{ {1}: {2} }};",control,subCall,captureGroup[0].Value);
		outputLines.Add(outputLine);
	}
	outputLines.Distinct().OrderBy(m=>m).Dump("server control calls transformed");
	return outputLines;
}