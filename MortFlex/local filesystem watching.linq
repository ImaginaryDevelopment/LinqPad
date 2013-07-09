<Query Kind="Program" />

IDictionary<string,string> _sites=new Dictionary<string,string>{
	{"Built",@"C:\Microsoft .Net 3.5 Framework\MortgageFlex Products\LoanQuest Origination\bin\release\"},
	{"Hosted",@"C:\Microsoft .Net 3.5 Framework\MortgageFlex Products\Common Framework\HOST\Mortgageflex.Services.Host.LoanQuest\Bin\"}
};
IDictionary<string,int> _fileChangeCounts= new Dictionary<string,int>();
bool debug=false;
void Main()
{
var toWatch=_sites.ToDictionary(k=>k.Key,v=>v.Value);

toWatch.Count.Dump("attempting to watch");
	
	var watchers=AttachHandlers(toWatch).Materialize();//prodWebDeploy,prod2WebDeploy}).ToArray();
	handlerCount.Dump("Handlers attached");
	while(Util.ReadLine()!=string.Empty)
	{
	
	}
	foreach(var item in watchers)
		item.Dispose();
	_fileChangeCounts.Dump();
}

static ushort handlerCount=0;
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

void NamedFsChangeHandler(object sender, System.IO.FileSystemEventArgs args,string key)
{

Func<string,bool> filter= s=>s.EndsWith(".dll",StringComparison.InvariantCultureIgnoreCase) || s.EndsWith(".bat", StringComparison.InvariantCultureIgnoreCase);
	//s.Contains(".")==false || s.EndsWith("config");
	var item=args.FullPath;
	  if(filter(item))
	  {
	  	_fileChangeCounts[item]=_fileChangeCounts.ContainsKey(item)? _fileChangeCounts[item]+1:1;
	  	item.Dump(key+":"+args.ChangeType.ToString());
	  }
}
void FsChangeHandler(object sender, System.IO.FileSystemEventArgs args)
{
	NamedFsChangeHandler(sender,args,"unknown");
}
// Define other methods and classes here