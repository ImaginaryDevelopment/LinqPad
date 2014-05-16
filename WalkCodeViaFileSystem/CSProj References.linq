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
	let isSlnProject=i.Contains("NonSln")==false && i.Contains("playground")==false
	let doc=XDocument.Load(i)
	let rootns=doc.Root.Name.Namespace
	let proj=doc.Element(rootns+"Project").DumpIf(x=>x==null,i+" has no project element")
	where proj!=null
	orderby isSlnProject
	select new{ Path=i, Doc=doc,RootNs=rootns,ProjNode=proj,IsSlnProject=isSlnProject}).ToArray();
var nonPackageReferences = from i in baseQuery
	
	from ig in i.ProjNode.Elements(i.RootNs+"ItemGroup")
	from r in ig.Elements(i.RootNs+"Reference")
	let hp=r.Element(i.RootNs+"HintPath")
	where hp!=null//&& hp.Value.Contains("packages")==false
	let absPath=hp.Value.Contains(":")?hp.Value: System.IO.Path.GetFullPath((System.IO.Path.GetDirectoryName(i.Path)+"\\"+hp.Value))
	let exists= System.IO.File.Exists(absPath) //TODO: solve for checking exists on network drives
	where !exists || hp.Value.Contains(":") || hp.Value.Contains("\\\\") 
	orderby i.IsSlnProject descending
	
	select new{hp.Value,reference=r.ToString(),i.IsSlnProject,i.Path,
		absPath,
		Exists=exists
	};
	
	nonPackageReferences
		.Where (pr => !pr.IsSlnProject)
		.GroupBy (pr => pr.Path,pr=>new{pr.Exists,Ref=Util.OnDemand(pr.Value,()=>new{pr.reference,pr.absPath})})	
		.Dump("nonpackage");
	
//	nonPackageReferences.Where (pr => pr.IsSlnProject==false)
//		.GroupBy (pr => pr.Path,pr=>new{ pr.Value,pr.reference,pr.Exists})	
//		.Dump("nonSlnNonPackage");
	
	
var references= from i in baseQuery
	from ig in i.ProjNode.Elements(i.RootNs+"ItemGroup")
	select new{Project=i.Path,Condition=ig.Attribute(XNamespace.None+"Condition"),Items= ig.Nodes().Cast<XElement>()};
	
	
references.Where (r => r.Items.Any (i => i.Attribute(XNamespace.None+"Include").Value.Contains("log4net")))
		.Select (r => new{r.Project,HibernateReferences=r.Items.Where (i => i.Attribute(XNamespace.None+"Include").Value.Contains("log4net")).ToArray()})
		//.Dump("log4net references")
		;
	references.Dump("references");