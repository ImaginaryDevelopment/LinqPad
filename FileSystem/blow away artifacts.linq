<Query Kind="Program" />

void Main()
{
	//clean bin and obj files, .suo and .user
	var targetPath= Util.ReadLine("Target Path to blow away?", "C:\\development");
	var doNotDescend = new[]{"$tf"};
	if(System.IO.Directory.Exists(targetPath)==false){
		"could not find path:".Dump(targetPath);
		return;
	}
	var deleted= new List<string>();
	foreach(var i in RecurseDirectory(targetPath,doNotDescend))
	{
		try
		{	        
			var isDir=System.IO.Directory.Exists(i);
		
			if(isDir==false && System.IO.File.Exists(i)==false)
			{
				i.Dump("skipping");
				continue;
			}
			if(isDir)
			{
				if(i.EndsWith("bin",StringComparison.InvariantCultureIgnoreCase)
					|| i.EndsWith("obj", StringComparison.InvariantCultureIgnoreCase)) //i.Contains("\\bin\\"))
				{
					//i.Dump("Deleting");
					System.IO.Directory.Delete(i,true);
					deleted.Add(i);
				}
			} else if(i.EndsWith(".suo") || i.EndsWith("proj.user")) {
				//i.Dump("Deleting");
				System.IO.File.Delete(i);
				deleted.Add(i);
			} 
		}
		catch (Exception ex)
		{
			ex.Dump("failed to delete:"+i);
		}
		
	}
		deleted.Dump("deleted");
}

// Define other methods and classes here
public IEnumerable<string> RecurseDirectory(string basePath,IEnumerable<string> folderBlacklist){
	if(System.IO.Directory.Exists(basePath)==false)
		yield break;
	foreach(var f in System.IO.Directory.GetFiles(basePath))
	{
		yield return f;
	}
	foreach(var d in System.IO.Directory.GetDirectories(basePath).Where (di => folderBlacklist.Any (b => di.EndsWith(b))==false)){
		yield return d;
		foreach(var i in RecurseDirectory(d,folderBlacklist))
			yield return i;
	}
}