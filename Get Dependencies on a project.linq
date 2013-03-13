<Query Kind="Program" />

// find out what projects depend on X project
Regex projectReferenceRegex=new Regex("Include=\"(.*\\.csproj)\">", RegexOptions.Compiled);
//Regex solutionFolder
const string container="{2150E333-8FDC-42A3-9474-1A3956D46DE8}";
Guid containerGuid=Guid.Parse(container);
void Main()
{

	var referenceProjects=new List<string>();
	var basePath=@"C:\Projects\psh\hpx";
	var sln=basePath+@"\solutions\AllApps.sln";
	var slnText= System.IO.File.ReadAllText(sln);
	var projects=GetProjects(basePath).ToArray();
	var autocomplete=projects.Select (p => System.IO.Path.GetFileNameWithoutExtension(p));
	var selectedProject=Util.ReadLine("Which project?","PaySpan.PayerPortal.WebSite",autocomplete);
	
	var selectedProjectFullPath=projects.First (p => p.AfterLastOrSelf("\\").Contains(selectedProject));
	selectedProjectFullPath.Dump("project path");
	foreach(var project in projects)
	{
		var text=System.IO.File.ReadAllText(project);
		if(text.Contains(selectedProject))
		{
			var depDirectory=System.IO.Path.GetDirectoryName(project);
			
			var q=new Queue<string>();
			foreach(var r in WalkReferences(text)
			.Select (t =>System.IO.Path.GetFullPath(System.IO.Path.Combine(depDirectory,t )))
			.Where (t => referenceProjects.Contains(t)==false))
			{
				if(System.IO.File.Exists(r)==false)
					throw new FileNotFoundException(r);
					
				q.Enqueue(r);
			}
			WalkDequeue(depDirectory, q,referenceProjects);
			
			
		}
	}
	referenceProjects.OrderBy (p => p).Select (p =>new{Name= System.IO.Path.GetFileNameWithoutExtension(p),Path=p}).ToArray().Dump("these depend on "+selectedProject);
}

void WalkDequeue(string depDirectory,Queue<string> q, IList<string> referenceProjects){
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
public IEnumerable<string> WalkReferences(string text){
	return projectReferenceRegex.Matches(text).Cast<Match>().Select (m => m.Groups[1].Value);
}
// Define other methods and classes here
public IEnumerable<string> GetProjects(string basePath)
{
	return System.IO.Directory.GetFiles(basePath,"*.csproj", SearchOption.AllDirectories);
}