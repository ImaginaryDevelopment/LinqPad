<Query Kind="Program" />

void Main()
{
	var sitwebDeploy=@"\\crprdnii1i7\sites$\Gtpm-init1\wwwroot\Site\";
	
	RecurseLocation(sitwebDeploy,string.Empty,new[]{"*.*"}).Where(f=>f.IsReadonly).Dump();
	
	
}

// Define other methods and classes here

public struct FileSummary
{
public string RelativePath;
public string FileName;
public bool IsReadonly;
public bool IsDirectory;
}

public static IEnumerable<FileSummary> RecurseLocation(string basePath, string relpath,IEnumerable<string> patterns)
{
		System.Diagnostics.Debug.Assert(relpath==string.Empty || relpath.StartsWith(basePath));
		//basePath.Dump("searching basepath");
		
		//var uriPath=(relpath.Length>1?"~"+ relpath.Substring(1):"");
		var absolute=System.IO.Path.Combine(basePath, relpath);
		
	foreach(var item in HandleFiles(patterns,absolute,basePath))
		yield return item;
	
		foreach(var dir in System.IO.Directory.GetDirectories(absolute))
		{
			
			foreach(var result in  RecurseLocation(basePath,dir,patterns))
			{ 
				yield return result;
			}
		}
		//
		//yield return result;
		
}
	
	public static IEnumerable<FileSummary> HandleFiles(IEnumerable<string> patterns, string absolute,string basePath)
	{
		foreach(var pattern in patterns)
		foreach(var file in System.IO.Directory.GetFiles(absolute,pattern))
		{
			yield return new FileSummary(){ RelativePath=absolute.Substring(absolute.Length-basePath.Length)+1,
											FileName=System.IO.Path.GetFileName(file), 
											IsReadonly=(System.IO.File.GetAttributes(basePath) & FileAttributes.ReadOnly)== FileAttributes.ReadOnly
											};	
		}
	
	}