<Query Kind="Program">
  <Connection>
    <ID>c9b76ea2-c7e7-4a9e-a336-ec4241c6dac5</ID>
    <Persist>true</Persist>
    <Server>rpsql2008r2dev</Server>
    <SqlSecurity>true</SqlSecurity>
    <Database>NOVA08RLS</Database>
    <UserName>WINRLS</UserName>
    <Password>AQAAANCMnd8BFdERjHoAwE/Cl+sBAAAA1WLKs9qc4USFiwcJ5tmkhgAAAAACAAAAAAADZgAAwAAAABAAAACrzDqjgelbVgZelHzxoUCkAAAAAASAAACgAAAAEAAAANaxhqC+PlSpfWS3MUIfULoIAAAAIqZ2P2kltAwUAAAAsZVmuOxGnoVr0wOGD/We2Jqm8Z4=</Password>
  </Connection>
</Query>

static ushort handlerCount=0;
Func<string,bool> fileFilter=s=>true;
static DateTime cutOff=DateTime.Today.AddHours(14);
void Main()
{
	var targetPath=@"\\vBCDApp1\c$\inetpub\logs\LogFiles\W3SVC1\";
	var latest= System.IO.Directory.EnumerateFiles(targetPath,"*.log").OrderByDescending(a=>a).First();
	System.IO.File.GetLastWriteTime(latest).Dump("last modified");
	ReparseLog(latest);
	latest.Dump("initial scan");
	if(!Util.ReadLine<bool>("watch for changes?",false))
		return;
	var watchers=AttachHandlers(new Dictionary<string,string>(){{"log",targetPath}} ).Materialize();//prodWebDeploy,prod2WebDeploy}).ToArray();
	handlerCount.Dump("Handlers attached");
	if(handlerCount>0)
	while(Util.ReadLine()!=string.Empty)
	{
	
	}
	foreach(var item in watchers)
		item.Dispose();
	
}

// Define other methods and classes here
IEnumerable<IDisposable> AttachHandlers(IDictionary<string,string> paths)
{
	foreach(var path in paths)
	{
		if(System.IO.Directory.Exists(path.Value)==false)
		{
			("Directory not found:"+path.Value).Dump(path.Key.ToString());
			yield break;
		}
		LINQPad.Util.RawHtml("<a href=\""+path.Value+"\" title=\""+path.Value+"\">link</a>").Dump(path.Key.ToString());
	
		var fs=new System.IO.FileSystemWatcher(path.Value);
		{
	  		fs.IncludeSubdirectories=true;
	  		fs.EnableRaisingEvents=true;
			fs.Changed+=(sender,args)=> NamedFsChangeHandler(sender,args,path.Key.ToString());  
			handlerCount++;
	  		fs.Deleted+= (sender,args)=> NamedFsChangeHandler(sender,args,path.Key.ToString());  
			handlerCount++;
	  		fs.Renamed+=(sender,args)=> NamedFsChangeHandler(sender,args,path.Key.ToString());  
			handlerCount++;
			fs.Created+=(sender,args)=> NamedFsChangeHandler(sender,args,path.Key.ToString());  
			handlerCount++;
		}
	  	yield return fs;
	
	}
}

void ReparseLog(string fullPath){
	
	var lines=fullPath.AsFilePath().ReadAllShared().SplitLines().AsEnumerable();

	lines=lines.Where(a=>a.Contains("192.168.102.158")==false);

	
	lines=lines.Where(a=>a.Contains("#")==false);
	var regex= BuildLogParserRegex();
	var q = from l in lines
		where l.IsNullOrEmpty()==false
		where l.Contains("rubyforge")==false
		let match= regex.Match(l)
		
		let year=int.Parse( match.Groups[1].Value)
		let month= int.Parse(match.Groups[2].Value)
		let day= int.Parse(match.Groups[3].Value)
		let hour= int.Parse(match.Groups[4].Value)
		let minute= int.Parse(match.Groups[5].Value)
		let second= int.Parse(match.Groups[6].Value)
		let dateTimeUtc=new DateTime(year,month,day,hour,minute,second, DateTimeKind.Utc)
		let localTime=dateTimeUtc.ToLocalTime()
		where localTime>cutOff
		orderby localTime descending
		select new { //DateTime=dateTimeRaw,
		LocalTime=localTime,
		//dateRaw= year +"_"+month+"_"+day,
		//HostIp= 		match.Groups[7].Value,
		Verb=			match.Groups[8].Value,
		RequestPath=	match.Groups[9].Value,
		RequestPort=match.Groups[10].Value,
		StatusCode=		match.Groups[12].Value,
		RequestIp=		match.Groups[11].Value,
			
			Raw= l};
	
	q.OrderByDescending(a=>a.LocalTime). Dump();
	cutOff=DateTime.Now.AddSeconds(-10);
}

Regex BuildLogParserRegex(){

	var regexDate= "(201[3-9])-([0-1][0-9])-([0-3][0-9])";
		var regexTime= "([0-2][0-9]):([0-5][0-9]):([0-5][0-9])";
		var regexIp = "[0-2][0-9][0-9]\\.[0-2][0-9][0-9]\\.[0-2][0-9][0-9]\\.[0-2][0-9][0-9]";
		var regexVerb = "[A-Z]+";
		var regexRequestPath = "/[^ ]*";
		var regexPort = "[1-9][0-9]*";
		var regexStatusCode= "[0-9]+";
		 var regex= new Regex(string.Format("{0} {1} ({2}) ({3}) ({4}) - ({5}) - ({2}) - ({6})", regexDate, regexTime, regexIp,regexVerb,regexRequestPath,regexPort,regexStatusCode));
		 return regex;
}

void NamedFsChangeHandler(object sender, System.IO.FileSystemEventArgs args,string key)
{


	//s.Contains(".")==false || s.EndsWith("config");
	var item=args.FullPath;
	  if(fileFilter(item))
	  {
	  	//_fileChangeCounts[item]=_fileChangeCounts.ContainsKey(item)? _fileChangeCounts[item]+1:1;
	  	item.Dump(key+":"+args.ChangeType.ToString());
		if(args.ChangeType== WatcherChangeTypes.Changed){
			"file changed, checking".Dump();
			ReparseLog(args.FullPath);
		}
	  }
}
void FsChangeHandler(object sender, System.IO.FileSystemEventArgs args)
{
	NamedFsChangeHandler(sender,args,"unknown");
}