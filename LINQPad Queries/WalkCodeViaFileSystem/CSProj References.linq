<Query Kind="Statements" />

// check hint paths for files that don't exist or are absolute

var baseDir=Util.ReadLine("Directory?",System.Environment.GetEnvironmentVariable("devroot"));
var projects= System.IO.Directory.GetFiles(baseDir,"*.*proj", SearchOption.AllDirectories);


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
	let hintPath=r.Element(i.RootNs+"HintPath")
	where hintPath!=null//&& hp.Value.Contains("packages")==false
	let absPath=hintPath.Value.Contains(":")?hintPath.Value: System.IO.Path.GetFullPath((System.IO.Path.GetDirectoryName(i.Path)+"\\"+hintPath.Value))
	let exists= System.IO.File.Exists(absPath) //TODO: solve for checking exists on network drives
	where !exists || hintPath.Value.Contains(":") || hintPath.Value.Contains("\\\\") 
	orderby i.IsSlnProject descending
	
	select new{hintPath.Value,reference=r.ToString(),i.IsSlnProject,i.Path,
		absPath,
		Exists=exists
	};
	
//	nonPackageReferences
//		.Where (pr => !pr.IsSlnProject)
//		.GroupBy (pr => pr.Path,pr=>new{pr.Exists,Ref=Util.OnDemand(pr.Value,()=>new{pr.reference,pr.absPath})})	
//		.Dump("nonpackage");
	
//	nonPackageReferences.Where (pr => pr.IsSlnProject==false)
//		.GroupBy (pr => pr.Path,pr=>new{ pr.Value,pr.reference,pr.Exists})	
//		.Dump("nonSlnNonPackage");
	
	
var references= from i in baseQuery
	from ig in i.ProjNode.Elements(i.RootNs+"ItemGroup")
	select new{Project=i.Path,Condition=ig.Attribute(XNamespace.None+"Condition"),Items= ig.Nodes().Cast<XElement>()};
	
	
references.Where (r => r.Items.Any (i => i.Attribute(XNamespace.None+"Include").Value.Contains("WebForms")))
		.Select (r => new{r.Project,HibernateReferences=r.Items.Where (i => i.Attribute(XNamespace.None+"Include").Value.Contains("WebForms")).ToArray()})
		.Dump("webforms references")
		;
	//references.Dump("references");