<Query Kind="Program" />

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
	var buildDefinition =buildPaths.First(x=>x.BuildName == targetBuild);
	var buildSrc = System.IO.Path.Combine( buildDefinition.buildFolder,"src"); // our builds have the msbuild log files going into the src folder for the specific project
	var productFolder = System.IO.Path.Combine(buildSrc,"Products");
	var slnFolder = System.IO.Path.Combine(productFolder,targetBuild.Contains("CV")?"CVS":"MarketOnce");
	Debug.Assert(System.IO.Directory.Exists(slnFolder),"Could not locate application slnFolder at "+slnFolder);
	foreach(var logFile in System.IO.Directory.EnumerateFiles(slnFolder,"*.log", SearchOption.AllDirectories)){ //consider caching previous log file locations and blacklisting debug and bin folders?
		var info = new System.IO.FileInfo(logFile);
		if(info.Length==0){
			info.Dump("Empty logfile"+logFile);
		}
		var cleaned=ReadBlock(System.IO.File.ReadAllLines(logFile),string.Empty);
		cleaned.Dump(logFile);
		
		
		//collect every mention of a .targets file then distinct by full path?
		//write out the file as html grouping based on indention?
		
		
	}
	
}

// Define other methods and classes here
public string ReadBlock (IEnumerable<string> lines,string prevIndent){
	var sb = new StringBuilder();
	sb.Append("<div>");
	foreach(var l in lines){ 
		var shallower =prevIndent!=string.Empty && l.StartsWith(prevIndent)==false;
		var deeper = !shallower && l.After(prevIndent).IsMatch(@"\s",false);
		
		if(!deeper && ! shallower) //same indention level
		{
			sb.AppendLine(System.Net.WebUtility.HtmlEncode(l));
			continue;
		}
		if(shallower){
			sb.Append("</div>");
			return sb.ToString();
		}
		// must be a deeper level
		var indentLevel = prevIndent==string.Empty? Regex.Match(l,@"\s+").Value: Regex.Match(l.After(prevIndent),@"\s+").Value; // get the whitespace after the current indent level if there is one
		sb.Append(ReadBlock(lines,indentLevel));
	}
	sb.Append("</div>");
	return sb.ToString();
}