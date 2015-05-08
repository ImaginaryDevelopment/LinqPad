<Query Kind="Statements" />

bool debug=false;
var baseDir=Util.ReadLine("Directory?",System.Environment.GetEnvironmentVariable("devroot"));
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
		let an =p.Nodes().Cast<XElement>().FirstOrDefault(n=>n.Name.LocalName=="AssemblyName")
		where an!=null
		orderby an.Value
		select new{ Project=i, PropertyGroupCondition=p.GetAttribValOrNull("Condition"),Value=an.Value};

//example
flat .Where(a=>a.PropertyGroupCondition.IsNullOrEmpty() || a.PropertyGroupCondition.Contains("Debug")==false)
.Dump();
//relational.Dump();