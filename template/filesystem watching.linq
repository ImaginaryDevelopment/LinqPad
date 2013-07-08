<Query Kind="Program" />

IDictionary<deploySites,string> _sites=new Dictionary<deploySites,string>{
	{deploySites.sitwebDeploy,@"\\gtpm-init1-sit\sites$\gtpm-init1\wwwroot\Site\" }, // @"\\crprdnii1i7\sites$\Gtpm-init1\wwwroot\Site\"},
	{deploySites.ditWebDeploy,@"\\crprdnii1i4\sites\Gtpm-init1\wwwroot\Site\"},
	{deploySites.ditsvcDeploy,@"\\crprdnii1i4\sites\Gtpm-init1\wwwroot\services\"},
	//{deploySites.uatwebDeploy,@""},
	{ deploySites.prodDeploy,@"\\crprdnii1h9\sites$\Gtpm-init1\wwwroot\"},
	{deploySites.prod2Deploy,@"\\crprdnii1h8\sites$\Gtpm-init1\wwwroot\"},
	{deploySites.unknown,@"\\crprdnii1g7\sites$\Gtpm\wwwroot\"},
};
bool debug=false;
void Main()
{
var toWatch=_sites.Except(_sites.Where(a=>a.Key== deploySites.prod2Deploy)).ToDictionary(k=>k.Key,v=>v.Value);

toWatch.Count.Dump("attempting to watch");
	var results=new System.Collections.Concurrent.BlockingCollection<dynamic>(toWatch.Count);
	foreach(var item in toWatch.AsParallel())
	{
		if(debug)("starting "+item.Key.ToString()).Dump();
		var dllPath=System.IO.Path.Combine(item.Value,@"site\bin\PSAT.WebApp.dll");
		if(System.IO.File.Exists(dllPath)==false)
		dllPath=System.IO.Path.Combine(item.Value,@"bin\PSAT.WebApp.dll");
		if(System.IO.File.Exists(dllPath))
		{
			var fs=new FileInfo(dllPath);
			
			Assembly r=null;
			Version aV=null;
			try
			{	        
			
				 r=System.Reflection.Assembly.ReflectionOnlyLoadFrom(dllPath);
				aV= r.GetName().Version;
				
			}
			catch (FileLoadException flex)
			{
				
				flex.Dump(item.Key.ToString());
			}
			var fi=FileVersionInfo.GetVersionInfo(dllPath);
			
			
			results.Add(new {Key=item.Key.ToString(),fs,fi,r,aV,item.Value});
		}
		
	}
	results.CompleteAdding();
	results.Dump();
	var watchers=AttachHandlers(toWatch).Materialize();//prodWebDeploy,prod2WebDeploy}).ToArray();
	handlerCount.Dump("Handlers attached");
	while(Util.ReadLine()!=string.Empty)
	{
	
	}
	foreach(var item in watchers)
		item.Dispose();
	
}
public enum deploySites
{
sitwebDeploy,
ditWebDeploy,
ditsvcDeploy,
//uatwebDeploy,
prodDeploy,
prod2Deploy,
unknown
}
static ushort handlerCount=0;
IEnumerable<IDisposable> AttachHandlers(IDictionary<deploySites,string> paths)
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

Func<string,bool> filter= s=> true;
	//s.Contains(".")==false || s.EndsWith("config");
	var item=args.FullPath;
	  if(filter(item))
	  item.Dump(key+":"+args.ChangeType.ToString());
}
void FsChangeHandler(object sender, System.IO.FileSystemEventArgs args)
{
	NamedFsChangeHandler(sender,args,"unknown");
}
// Define other methods and classes here