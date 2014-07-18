<Query Kind="Program" />

void Main()
{
	var buildServer = Environment.GetEnvironmentVariable("servers").Dump().Split(new []{";"},StringSplitOptions.RemoveEmptyEntries).Dump().FirstOrDefault(c=>c.Contains("build"));
	var localbase=@"\\"+buildServer+@"\c$\";
	var remotebase=@"C:\";
	var relative=@"builds\1";
	foreach(var item in RecurseFiles(localbase+relative))
	{
		var remotePath= remotebase+item.After(localbase);
		if(remotePath.Length>255)
			new{item,remotePath,remotePath.Length}.Dump();
			
	}
	
}

// Define other methods and classes here
IEnumerable<string>RecurseFiles(string basePath){
	foreach(var i in System.IO.Directory.GetFiles(basePath)){
		yield return i;
	}
	foreach(var d in System.IO.Directory.GetDirectories(basePath)){
		foreach(var f in RecurseFiles(d))
			yield return f;
	}
}