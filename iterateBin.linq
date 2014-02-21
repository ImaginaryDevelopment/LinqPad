<Query Kind="Program" />

void Main()
{
	
	var srcPath=System.Environment.GetEnvironmentVariable("devroot")+@"\Products\CVS\Member\CVS.Member.Web4\bin";
	var q=from i in System.IO.Directory.EnumerateFiles(srcPath,"*.dll")
	let fileName=System.IO.Path.GetFileName(i)
	let buildInfo=new System.IO.FileInfo(i)
	let builtAssInfo = TryReflectionLoad(i)
	let runtime= new{ Built=builtAssInfo!=null? builtAssInfo.ImageRuntimeVersion:null}
	let targetFrame= new {Built= builtAssInfo!=null? builtAssInfo.GetCustomAttributes(true)
		//.OfType<System.Runtime.Versioning.TargetFrameworkAttribute>()
		//.FirstOrDefault()
		:null}
	
	let creation= buildInfo.CreationTime
	let modification=buildInfo.LastWriteTime
	let size= buildInfo.Length
	let versions= FileVersionInfo.GetVersionInfo(i)
	let fileVersion = versions.FileVersion
	let productVersion= versions.ProductVersion
	orderby fileName.StartsWith("Oceanside") descending,buildInfo.LastWriteTimeUtc descending, fileName
	select new{Item=buildInfo.Name, //LINQPad.Util.HighlightIf(i,_=>buildInfo.CreationTimeUtc!=hostInfo.CreationTimeUtc || buildInfo.Length!=hostInfo.Length),
				Modification=modification,
				//ModDifference= deployDifferenceMinutes,
				runtime,targetFrame,
				FileVersion =fileVersion ,
				ProductVersion= productVersion,
				Size=size,
				Creation= creation //Util.HighlightIf(creation,a=>a.Built!=a.Hosted),
				};
				q.Dump();
}

// Define other methods and classes here
Assembly TryReflectionLoad(string path){
	try
	{	        
		return System.Reflection.Assembly.LoadFrom(path);
	}
	catch (Exception ex)
	{
		if(path.Contains("GdPicture")==false)
		ex.Dump();
		return null;
	}
}