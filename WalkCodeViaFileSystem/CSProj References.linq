<Query Kind="Statements" />

//bool debug=false;
var baseDir=Util.ReadLine("Directory?",@"C:\Development\Products\CVS");
var projects= System.IO.Directory.GetFiles(baseDir,"*.*proj", SearchOption.AllDirectories);
//sample projFile
//XDocument.Load(projects.Take(1).Single ( )).DumpFormatted(projects.Take(1).Single ());
var csProjects=projects.Where(f=>f.EndsWith(".csproj") && f.EndsWith("WordsMatching.csproj")==false && f.Contains("test", StringComparison.InvariantCultureIgnoreCase)==false);//.Take(2);
var baseQuery=(from i in csProjects
	let doc=XDocument.Load(i)
	let rootns=doc.Root.Name.Namespace
	let proj=doc.Element(rootns+"Project").DumpIf(x=>x==null,i+" has no project element")
	where proj!=null
	select new{ Path=i, Doc=doc,RootNs=rootns,ProjNode=proj}).ToArray();
var references= from i in baseQuery
	from ig in i.ProjNode.Elements(i.RootNs+"ItemGroup")
	
	select new{Project=i.Path,Condition=ig.Attribute(XNamespace.None+"Condition"),Items= ig.Nodes().Cast<XElement>()};
	references.Where (r => r.Items.Any (i => i.Attribute(XNamespace.None+"Include").Value.Contains("log4net")))
		.Select (r => new{r.Project,HibernateReferences=r.Items.Where (i => i.Attribute(XNamespace.None+"Include").Value.Contains("log4net")).ToArray()})
		.Dump("log4net references")
		;
	references.Dump("references");

