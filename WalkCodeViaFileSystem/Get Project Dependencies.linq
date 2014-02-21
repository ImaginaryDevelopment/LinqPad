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
//regex to find all project references
Regex projectReferenceRegex=new Regex("Include=\"(.*\\.csproj)\">", RegexOptions.Compiled);

//solutionFolder
const string container="{2150E333-8FDC-42A3-9474-1A3956D46DE8}";
Guid containerGuid=Guid.Parse(container);
void Main()
{

	var referenceProjects=new List<DependencyInfo>();
	var basePath=System.Environment.GetEnvironmentVariable("devroot");
	string sln;
	using(var ofd= new OpenFileDialog()){
		ofd.InitialDirectory=basePath;
		ofd.DefaultExt="SolutionFiles(*.sln)|*.sln";
		if(ofd.ShowDialog()!= DialogResult.OK){
			"cancelled, aborting".Dump();
			return;
		}
		
		sln=ofd.FileName.Dump("slnpath");
	}
	var slnText= System.IO.File.ReadAllText(sln);
	var projects=GetProjects(basePath).ToArray();
	var autocomplete=projects.Select (p => System.IO.Path.GetFileNameWithoutExtension(p));
	var selectedProject=Util.ReadLine("Which project?",string.Empty,autocomplete.Dump("options"));
	var selectedProjectFullPath=projects.First (p => p.AfterLastOrSelf("\\").IndexOf(selectedProject,StringComparison.InvariantCultureIgnoreCase)>=0);
	//adjust for autocomplete casing fail
	selectedProject=System.IO.Path.GetFileNameWithoutExtension( selectedProjectFullPath);
	var selectedText=System.IO.File.ReadAllText(selectedProjectFullPath);
	
	var directReferences=WalkReferences(selectedText);
	referenceProjects.AddRange(directReferences.Select (r => new DependencyInfo(selectedProjectFullPath,r,0)));
	referenceProjects.Dump("first pass");
	var walked=new List<string>(){selectedProjectFullPath};
	var searching=true;
	int depth=1;
	while(searching){
		var addedItem=false;
		var toWalk=referenceProjects.Where (p => p.Exists &&  walked.Contains(p.FullPath)==false ).ToArray();
		
		foreach(var walk in toWalk.ToArray()){
			
			var badRefence=toWalk.FirstOrDefault (w => w.Exists==false);
			if(badRefence!=null)
			{
				badRefence.Dump("bad");
				return;
			}
			walked.Add(walk.FullPath);
			
			var projectText=System.IO.File.ReadAllText(walk.FullPath);
			var refDirectory=System.IO.Path.GetDirectoryName(walk.FullPath);
			var references=WalkReferences(projectText).Select (wr => new DependencyInfo(walk.FullPath,wr,depth)).Where (wr =>walked.Contains(wr.FullPath)==false );
			//references.Dump("found by walking:"+walk.Name);
			foreach(var r in references.Where (r =>referenceProjects.Any (p => p.FullPath==r.FullPath)==false )){
				referenceProjects.Add(r);
				addedItem=true;
			}
		}
		
		if(!addedItem)
		{
			"done searching".Dump();
			searching=false;
		}
		depth++;
	}
	
	referenceProjects.OrderBy (p => p.Name).ThenBy (p => p.Depth).Dump(selectedProject+" depends on");
	
	
}


public IEnumerable<string> WalkReferences(string text){
	return projectReferenceRegex.Matches(text).Cast<Match>().Select (m => m.Groups[1].Value);
}

public IEnumerable<string> GetProjects(string basePath)
{
	return System.IO.Directory.GetFiles(basePath,"*.csproj", SearchOption.AllDirectories);
}
public class DependencyInfo{
readonly string _baseDirectory;
	public string Name{get;private set;}
	public string RelativeName{get;private set;}
	public string ReferencedBy{get;private set;}
	public int Depth{get;set;}
	public string FullPath{get;private set;}
	public bool Exists{get;private set;}
	public DependencyInfo(string referencedBy,string relativeName,int depth){
		if(System.IO.File.Exists( referencedBy)==false)
			throw new FileNotFoundException(referencedBy);
		_baseDirectory=System.IO.Path.GetDirectoryName(referencedBy);
		ReferencedBy=referencedBy;
		RelativeName=relativeName;
		Depth=depth;
		FullPath=System.IO.Path.GetFullPath(System.IO.Path.Combine(_baseDirectory,relativeName));
		Exists=System.IO.File.Exists(FullPath);
		Name=System.IO.Path.GetFileName(FullPath);
	}
}