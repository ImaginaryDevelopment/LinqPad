<Query Kind="Program" />

Regex projectReferenceRegex=new Regex("Include=\"(.*\\.csproj)\">", RegexOptions.Compiled);
//Regex solutionFolder
const string container="{2150E333-8FDC-42A3-9474-1A3956D46DE8}";
Guid containerGuid=Guid.Parse(container);
void Main()
{
	var referenceProjects=new List<string>();
	var basePath=@"C:\Projects\trunk\hpx";
	var sln=@"C:\Projects\trunk\hpx\solutions\AllApps.sln";
	var slnText= System.IO.File.ReadAllText(sln);
	
	foreach(var dependentProject in GetProjects(basePath).ToArray())
	{
		var text=System.IO.File.ReadAllText(dependentProject);
		if(text.Contains("WS-NPSQueryBroker"))
		{
			var depDirectory=System.IO.Path.GetDirectoryName(dependentProject);
			//depDirectory.Dump("dep directory");
			dependentProject.Dump();
			var q=new Queue<string>();
			foreach(var r in WalkReferences(text)
			.Select (t =>System.IO.Path.GetFullPath(System.IO.Path.Combine(depDirectory,t )))
			.Where (t => referenceProjects.Contains(t)==false))
			{
			if(System.IO.File.Exists(r)==false)
					throw new FileNotFoundException(r);
					
				q.Enqueue(r);
			}
			while(q.Any ())
			{
				var r=q.Dequeue();
				
				//r.Dump("walking");
				if(System.IO.File.Exists(r)==false)
					throw new FileNotFoundException(r);
					if(referenceProjects.Contains(r)==false)
					{
						referenceProjects.Add(r);
					}
				depDirectory=System.IO.Path.GetDirectoryName(r);
				var sText=System.IO.File.ReadAllText(r);
				
				foreach(var item in WalkReferences(sText).Select (t =>System.IO.Path.GetFullPath(System.IO.Path.Combine(depDirectory,t )))
					.Where (t => referenceProjects.Contains(t)==false).Distinct())
				{
				if(System.IO.File.Exists(item)==false)
					throw new FileNotFoundException(item);
					q.Enqueue(item);
				}
				
			
			}
			
			
		}
	}
	referenceProjects.OrderBy (p => p).Select (p =>new{Name= System.IO.Path.GetFileNameWithoutExtension(p),Path=p}).ToArray().Dump();
}
public IEnumerable<string> WalkReferences(string text){
	return projectReferenceRegex.Matches(text).Cast<Match>().Select (m => m.Groups[1].Value);
}
// Define other methods and classes here
public IEnumerable<string> GetProjects(string basePath)
{
	return System.IO.Directory.GetFiles(basePath,"*.csproj", SearchOption.AllDirectories);
}