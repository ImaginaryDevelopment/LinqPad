<Query Kind="Statements" />

//bool debug=false;
var baseDir=Util.ReadLine("Directory?",System.Environment.GetEnvironmentVariable("devroot"));
var projects= System.IO.Directory.GetFiles(baseDir,"*.*proj", SearchOption.AllDirectories);
var targetPropName = "TargetFrameworkVersion";
//sample projFile
//XDocument.Load(projects.Take(1).Single ( )).DumpFormatted(projects.Take(1).Single ());
var csProjects = projects.Where(f =>!f.EndsWith("vdproj") && f.EndsWith("WordsMatching.csproj") == false && f.Contains("test", StringComparison.InvariantCultureIgnoreCase) == false);//.Take(2);

Func<string, XDocument> loadProjectXml = p =>
{
	try
	{
		return XDocument.Load(p);
	}
    catch (Exception ex)
	{
		ex.Dump(p);
		throw;
	}
};

var baseQuery=(from i in csProjects
	let doc= loadProjectXml(i)
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
		.Where (p => p.Attribute(XNamespace.None+ "Condition")==null).SelectMany (p => p.Elements().Select (xe =>new{ xe.Name.LocalName,
		Condition=xe.Attribute(XNamespace.None+"Condition"), xe.Value})),
			ConditionalGroups=proj.Elements(doc.Root.Name.Namespace+"PropertyGroup") //};
		.Where (p => p.Attribute(XNamespace.None+ "Condition")!=null).Select (p => new{Condition=p.Attribute(XNamespace.None+"Condition"),Properties=p.Elements().Select (xe =>new{ xe.Name.LocalName,
		Condition=xe.Attribute(XNamespace.None+"Condition"), xe.Value})} )};
var flat= from i in csProjects
		let doc=XDocument.Load(i)
		let rootns=doc.Root.Name.Namespace
		let proj=doc.Element(rootns+"Project")
		let bogus= proj==null? doc.DumpFormatted(i):null
		where proj!=null
		from p in proj.Elements(rootns+"PropertyGroup")
		from prop in p.Elements().Cast<XElement>()
		where prop != null
		select new{prop.Value,Name=i.AfterLastOrSelf("\\"), Project=i, PropertyGroupCondition=p.Attribute(XNamespace.None+"Condition"),prop.Name.LocalName};

//example
flat
	.Where (f => f.LocalName==targetPropName)
	.Where (f => f.Value.IsNullOrEmpty() || f.Value!="v4.5.1")
	.OrderByDescending (f => f.Value)
	.Dump("specific prop");
			
relational.Dump("relational");
flat.Dump("all flat");