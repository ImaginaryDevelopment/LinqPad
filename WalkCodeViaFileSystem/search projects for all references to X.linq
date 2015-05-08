<Query Kind="Statements" />

// look for all references that contain a specific text

var baseDir=Util.ReadLine("Directory?",System.Environment.GetEnvironmentVariable("devroot"));

var referenceToSearchFor = Util.ReadLine("reference?");

var projects= System.IO.Directory.GetFiles(baseDir,"*.*proj", SearchOption.AllDirectories);


var csProjects=projects.Where(f=>f.EndsWith(".csproj") 
		&& f.Contains("test", StringComparison.InvariantCultureIgnoreCase)==false //don't check testing projects
		)
		//.Dump("csprojects")
		;
csProjects.Count ().Dump("checking projects");

var baseQuery=(from i in csProjects	
	let isSlnProject=i.Contains("NonSln")==false && i.Contains("playground")==false
	let doc=XDocument.Load(i)
	let rootns=doc.Root.Name.Namespace
	let proj=doc.Element(rootns+"Project").DumpIf(x=>x==null,i+" has no project element")
	where proj!=null
	orderby isSlnProject
	select new{ Path=i, Doc=doc,RootNs=rootns,ProjNode=proj,IsSlnProject=isSlnProject}).ToArray();
	
var references= from i in baseQuery
	from ig in i.ProjNode.Elements(i.RootNs+"ItemGroup")
	select new{Project=i.Path,Condition=ig.Attribute(XNamespace.None+"Condition"),Items= ig.Nodes().Cast<XElement>()};
	
references.Where (r => r.Items.Any (i => i.Attribute(XNamespace.None+"Include").Value.Contains(referenceToSearchFor, StringComparison.InvariantCultureIgnoreCase )))
		.Select (r => new{r.Project,InterestedReferences=r.Items.Where (i => i.Attribute(XNamespace.None+"Include").Value.Contains(referenceToSearchFor,StringComparison.InvariantCultureIgnoreCase )).ToArray()})
		.Dump(referenceToSearchFor+" references")
		;
	//references.Dump("references");