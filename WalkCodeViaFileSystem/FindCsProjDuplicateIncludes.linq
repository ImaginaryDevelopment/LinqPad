<Query Kind="Statements" />

//bool debug=false;
var baseDir=Util.ReadLine("Directory?",System.Environment.GetEnvironmentVariable("devroot"));

var projects=baseDir.AsDirPath().GetFiles("*.*proj").ToArray();
projects.Select(p=>new{Name=System.IO.Path.GetFileName(p),FullPath=p}).Dump();
//how do we handle projects with the same name?
var targetProject = Util.ReadLine("Project?",null,projects.Select(p=>System.IO.Path.GetFileName(p)));
//sample projFile
//XDocument.Load(projects.Take(1).Single ( )).DumpFormatted(projects.Take(1).Single ());
var fullPath=projects.First(a=>a.EndsWith(targetProject, StringComparison.CurrentCultureIgnoreCase));
var doc=XDocument.Load(fullPath);
var rootns = doc.Root.Name.Namespace;
var proj=doc.Element(rootns+"Project").DumpIf(x=>x==null,fullPath+" has no project element");

if(proj==null)
	throw new InvalidOperationException("no project element found");
LINQPad.Util.ClearResults();
var baseQuery=new{ Path=fullPath, Doc=doc,RootNs=rootns,ProjNode=proj};
var references= from ig in baseQuery.ProjNode.Elements(baseQuery.RootNs+"ItemGroup")
		let items = ig.Nodes().Cast<XElement>().ToArray()
		let distincts = items.Select(i=>i.GetAttribValOrNull("Include")).Where(i=>i.IsNullOrEmpty()==false).Distinct()
	select new{Condition=ig.Attribute(XNamespace.None+"Condition"),
		Duplicates= items.GroupBy(i=>i.GetAttribValOrNull("Include")).Where(x=>x.Count()>1).SelectMany(x=>x).DistinctBy(x=>x.ToString()),
		Distinct=distincts.Count(),
		Total=items.Count(),
		References=items.Where(r=>r.Name.LocalName=="Reference").OrderBy(x=>x.GetAttribValOrNull("Include")).ToArray(),
		Items= items};
	
	
references.Where(x=>x.Distinct!=x.Total).Dump(baseQuery.Path,2);
projects.Select(p=>new{Name=System.IO.Path.GetFileName(p),FullPath=p}).Dump("availableProjects under requested Directory");
