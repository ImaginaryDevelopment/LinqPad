<Query Kind="Statements" />

//bool debug=false;
var baseDir=Util.ReadLine("Directory?",System.Environment.GetEnvironmentVariable("devroot"));
var projects= System.IO.Directory.GetFiles(baseDir,"*.*proj", SearchOption.AllDirectories);
//sample projFile
//XDocument.Load(projects.Take(1).Single ( )).DumpFormatted(projects.Take(1).Single ());

var csProjects=projects.Where(f=>f.EndsWith(".csproj") 
		&& f.Contains("test", StringComparison.InvariantCultureIgnoreCase)==false //don't check testing projects
		//&& nonSlnProjects.All(non=>f.EndsWith(non)==false)
		
		);//.Take(2);
csProjects.Count ().Dump("checking projects");
var baseQuery=(from i in csProjects	
	let isSlnProject=i.Contains("NonSln", StringComparison.InvariantCultureIgnoreCase)==false && i.Contains("playground", StringComparison.InvariantCultureIgnoreCase)==false
	let doc=XDocument.Load(i)
	let rootns=doc.Root.Name.Namespace
	let proj=doc.Element(rootns+"Project").DumpIf(x=>x==null,i+" has no project element")
	where proj!=null
	orderby isSlnProject
	select new{ Path=i, Doc=doc,RootNs=rootns,ProjNode=proj,IsSlnProject=isSlnProject}).ToArray();
	//baseQuery.Dump();
var nonPackageReferences = from i in baseQuery
	
	from ig in i.ProjNode.Elements(i.RootNs+"Import")
	let iProject=ig.GetAttribValOrNull("Project")
	where iProject!=null//&& hp.Value.Contains("packages")==false
	
	
	orderby i.IsSlnProject descending
	
	select new{iProject,Condition=ig.GetAttribValOrNull("Condition"),i.IsSlnProject,i.Path};// new{ig,iProject,reference=iProject.ToString(),i.IsSlnProject,i.Path,	};
	
	nonPackageReferences
		.Where (pr => pr.IsSlnProject && pr.iProject.Contains("Web"))
		//.GroupBy (pr => pr.Path,pr=>pr.iProject)	
		.Dump("nonpackage");