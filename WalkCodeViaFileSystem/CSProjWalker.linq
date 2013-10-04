<Query Kind="Statements" />

bool debug=false;
var baseDir=Util.ReadLine("Directory?",@"C:\Microsoft .Net 3.5 Framework\MORTGAGEFLEX PRODUCTS");
var projects= System.IO.Directory.GetFiles(baseDir,"*.*proj", SearchOption.AllDirectories);
//sample projFile
//XDocument.Load(projects.Take(1).Single ( )).DumpFormatted(projects.Take(1).Single ());
var csProjects=projects.Where(f=>f.EndsWith(".csproj"));//.Take(2);
var baseQuery=(from i in csProjects
	let doc=XDocument.Load(i)
	let rootns=doc.Root.Name.Namespace
	let proj=doc.Element(rootns+"Project").DumpIf(x=>x==null,i+" has no project element")
	where proj!=null
	select new{ Path=i, Doc=doc,RootNs=rootns,ProjNode=proj}).ToArray();
var references= from i in baseQuery
	from ig in i.ProjNode.Elements(i.RootNs+"ItemGroup")
	
	select new{Project=i.Path,Condition=ig.Attribute(XNamespace.None+"Condition"),Items= ig.Nodes().Cast<XElement>()};
	references.Where (r => r.Items.Any (i => i.Attribute(XNamespace.None+"Include").Value.Contains("Hibernate")))
		.Select (r => new{r.Project,HibernateReferences=r.Items.Where (i => i.Attribute(XNamespace.None+"Include").Value.Contains("Hibernate")).ToArray()})
		.Dump("hibernate references")
		;
	//references.Dump("references");
//var runtime=from i in baseQuery
//	from r in i.ProjNode.Elements(i.RootNs+"runtime")
//	where r!=null
//	let abNs=r.GetNamespaceOfPrefix("urn")
//	let ab=r.Element(abNs+"assemblyBinding")
//	select new{r,ab};
//	runtime.Dump();
var relational= from i in csProjects
		let doc=XDocument.Load(i)
		let proj=doc.Element(doc.Root.Name.Namespace+"Project").DumpIf(x=>x==null,i+" has no project element")
		let bogus= proj==null? doc.DumpFormatted(i):null
		where proj!=null
		select new{i,Properties=proj.Elements(doc.Root.Name.Namespace+"PropertyGroup") //};
		.Where (p => p.Attribute(XNamespace.None+ "Condition")==null).SelectMany (p => p.Nodes().Cast<XElement>().Select (xe =>new{ xe.Name.LocalName,
		Condition=xe.Attribute(XNamespace.None+"Condition"), xe.Value})),
			ConditionalGroups=proj.Elements(doc.Root.Name.Namespace+"PropertyGroup") //};
		.Where (p => p.Attribute(XNamespace.None+ "Condition")!=null).Select (p => new{Condition=p.Attribute(XNamespace.None+"Condition"),Properties=p.Nodes().Cast<XElement>().Select (xe =>new{ xe.Name.LocalName,
		Condition=xe.Attribute(XNamespace.None+"Condition"), xe.Value})} )};
var flat= from i in csProjects
		let doc=XDocument.Load(i)
		let rootns=doc.Root.Name.Namespace
		let proj=doc.Element(rootns+"Project")
		let bogus= proj==null? doc.DumpFormatted(i):null
		where proj!=null
		from p in proj.Elements(rootns+"PropertyGroup")
		from prop in p.Nodes().Cast<XElement>()
		select new{ Project=i, PropertyGroupCondition=p.Attribute(XNamespace.None+"Condition"),prop.Name.LocalName,prop.Value};

//example
flat.Where (f => f.LocalName=="VisualStudioVersion").Dump();
			
relational.Dump();
flat.Dump();