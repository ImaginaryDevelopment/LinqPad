<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Security.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\Accessibility.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Configuration.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Deployment.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.Serialization.Formatters.Soap.dll</Reference>
  <Namespace>System.Windows.Forms</Namespace>
</Query>

// find out what projects depend on X project

/* walks all csproj files in the sln directory and below looking for projects that refer directly to the selected project */

/* it does NOT walk the solution file, since there can be projects with project references that aren't part of the solution.
  this can be accounted for better, but has not been */

readonly Regex projectReferenceRegex=new Regex("Include=\"(.*\\.csproj)\">", RegexOptions.Compiled);

//Regex solutionFolder
readonly MissingFileBehavior missingFileBehavior = MissingFileBehavior.Warn;

const string container="{2150E333-8FDC-42A3-9474-1A3956D46DE8}";
Guid containerGuid=Guid.Parse(container);

void Main()
{
	var referenceProjects = new List<string>();
	var basePath = TrySelectStartDirectory(System.Environment.GetEnvironmentVariable("devroot"));
	if(basePath.IsNullOrEmpty())
	{
		"cancelled, aborting".Dump();
		return;
	}
	
	basePath.Dump("searching from ");
	
	var projects=GetProjects(basePath).ToArray();
	var autocomplete=projects.Select (p => Path.GetFileNameWithoutExtension(p)).ToArray();
	var selectedProject=Util.ReadLine("Which project?",autocomplete[0],autocomplete.Dump("projects"));
	
	var selectedProjectFullPath=projects.First (p => p.AfterLastOrSelf("\\").Contains(selectedProject));
	selectedProjectFullPath.Dump("project path");
	var toWarn = new List<object>();
	foreach(var project in projects)
	{
		var text=System.IO.File.ReadAllText(project); 
		
		
		if(!text.Contains(selectedProject)) // simple text contains, very flawed if your project happens to be named something short and simple like (configuration)
			continue;
		
		
		var xml = XElement.Parse(text);
		var itemGroups = xml.Descendants(xml.Name.Namespace+"ItemGroup");
		var refs = xml.Descendants(xml.Name.Namespace+"Reference").Select( r=> new{ r,Include=r.GetAttribValOrNull("Include")});
		var projRefs = xml.Descendants(xml.Name.Namespace+"ProjectReference").Select( r=> new{r, Include=r.Descendants(xml.Name.Namespace+"Name").Select(x =>x.Value).FirstOrDefault()});
		if(!refs.Any(r=>r.Include==selectedProject) && !projRefs.Any(pr => pr.Include == selectedProject))
		{
			toWarn.Add(new{project,refs,projRefs,text});
			continue;
		}
		new{ project,text=Util.OnDemand("full proj text",()=>text),refs,projRefs}.Dump("contains selected!");
		var depDirectory=System.IO.Path.GetDirectoryName(project);
		
		var q=new Queue<string>();
		foreach(var r in WalkReferences(text)
		.Select (t =>System.IO.Path.GetFullPath(System.IO.Path.Combine(depDirectory,t )))
		.Where (t => referenceProjects.Contains(t)==false))
		{
			if(System.IO.File.Exists(r)==false)
			{
				if(missingFileBehavior == MissingFileBehavior.Throw)
					throw new FileNotFoundException(project,r);
				Util.Highlight("Warning: Could not load " + r + " from " + project).Dump();
			}	
			q.Enqueue(r);
		}
		
		WalkDequeue(depDirectory, q,referenceProjects);	
		
	}
	
	//display final results
	referenceProjects
		.OrderBy (p => p)
		.Select (p =>new{Name= Path.GetFileNameWithoutExtension(p),Path=p})
		.ToArray()
		.Dump("these depend on "+selectedProject);
	Util.Highlight(toWarn).Dump("Warning: contains the text but doesn't match the actual reference text");
}

void WalkDequeue(string depDirectory,Queue<string> q, IList<string> referenceProjects)
{
	while(q.Any ())
	{
		var r=q.Dequeue();
		
		//r.Dump("walking");
		if(System.IO.File.Exists(r)==false)
		{
			if(missingFileBehavior==MissingFileBehavior.Throw)
			throw new FileNotFoundException(r);
			
			Util.Highlight("Warning: could not load file "+ r + " from depDirectory " + depDirectory).Dump();
			continue;
		}
		
		if(referenceProjects.Contains(r)==false)
		{
			referenceProjects.Add(r);
		}
		
		depDirectory = System.IO.Path.GetDirectoryName(r);
		var sText = System.IO.File.ReadAllText(r);
		var matches = WalkReferences(sText).ToArray();
		//(new{ r, matches}).Dump();
		
		foreach(var item in matches.Select (t =>System.IO.Path.GetFullPath(System.IO.Path.Combine(depDirectory,t )))
			.Where (t => referenceProjects.Contains(t) == false).Distinct())
		{
			if(System.IO.File.Exists(item) == false){
				if(missingFileBehavior == MissingFileBehavior.Throw)
					throw new FileNotFoundException(item);
				Util.Highlight("Warning: could not load file " + item).Dump();
			} else
			q.Enqueue(item);
		}
	}
}

//walk project text searching for project references
public IEnumerable<string> WalkReferences(string text)
{
	return projectReferenceRegex
		.Matches(text)
		.Cast<Match>()
		.Select (m => m.Groups[1].Value);
}

public IEnumerable<string> GetProjects(string basePath)
{
	return Directory.GetFiles(basePath,"*.csproj", SearchOption.AllDirectories);
}

public string TrySelectStartDirectory(string basePath)
{
	string sln; // note: this just selects what directory to start the search from

	using(var ofd= new OpenFileDialog()){
		ofd.InitialDirectory=basePath;
		ofd.Title="Locate a solution folder to use as the base path for searching";
		ofd.DefaultExt="SolutionFiles(*.sln)|*.sln";
		if(ofd.ShowDialog()!= DialogResult.OK){
			return null;
		}
		sln=ofd.FileName.Dump("slnpath");
	}
	//var slnText= System.IO.File.ReadAllText(sln).Dump();
	return System.IO.Path.GetDirectoryName(sln);
}

public enum MissingFileBehavior
{
	Throw,
	Warn
}