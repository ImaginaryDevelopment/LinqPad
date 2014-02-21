<Query Kind="Statements" />

//bool debug=false;
var baseDir=Util.ReadLine("Directory?",System.Environment.GetEnvironmentVariable("devroot"));

var projects= System.IO.Directory.GetFiles(baseDir,"*.*proj", SearchOption.AllDirectories);
//sample projFile
//XDocument.Load(projects.Take(1).Single ( )).DumpFormatted(projects.Take(1).Single ());
var csProjects=projects.Where(f=>f.EndsWith(".csproj") 
	&& f.Contains("NonSln")==false 
	&& f.EndsWith("WordsMatching.csproj")==false 
	&& f.Contains("test", StringComparison.InvariantCultureIgnoreCase)==false);//.Take(2);
	
var baseQuery=(from i in csProjects
	let doc=XDocument.Load(i)
	let rootns=doc.Root.Name.Namespace
	let proj=doc.Element(rootns+"Project").DumpIf(x=>x==null,i+" has no project element")
	where proj!=null
	select new{ Path=i, Doc=doc,RootNs=rootns,ProjNode=proj}).ToArray();
	
var references= from i in baseQuery
	let isWebProject=i.ProjNode.Elements(i.RootNs+"Import").Select (pn => pn.GetAttribValOrNull("Project")).Any (pn => pn.Contains("Microsoft.WebApplication.targets"))
	from ig in i.ProjNode.Elements(i.RootNs+"ItemGroup")
	from r in ig.Elements().Where (x => x.Name.LocalName=="Reference" || x.Name.LocalName=="ProjectReference")
	let inc = r.GetAttribValOrNull("Include")
	where inc!=null
	let IsCopyLocal=r.Elements().Where (x => x.Name.LocalName=="Private").Select (x => x.Value).FirstOrDefault ()
	where inc.Contains("Oceanside") || inc.Contains("CVS") || IsCopyLocal!=null
	let specificVersion=r.Elements().Where(x => x.Name.LocalName=="SpecificVersion").Select (x => x.Value).FirstOrDefault ()
	let hintPath=r.Elements().Where (x => x.Name.LocalName=="HintPath").Select (x => x.Value).FirstOrDefault ()
	orderby inc.Contains("CVS") descending ,inc.Contains("Oceanside") descending,r.Name.LocalName descending, IsCopyLocal descending
	select new{name=inc.BeforeOrSelf(",").AfterLastOrSelf("\\"),r.Name.LocalName,IsCopyLocal,isWebProject,hintPath, Project=i.Path,Condition=ig.Attribute(XNamespace.None+"Condition"), inc,specificVersion,r};
	//should have copy local true for all if project contains import of target  <Import Project="$(VSToolsPath)\WebApplications\Microsoft.WebApplication.targets" Condition="'$(VSToolsPath)' != '' AND Exists('$(VSToolsPath)\WebApplications\Microsoft.WebApplication.targets')" />
	references.Where (r => r.LocalName!="ProjectReference" && (r.inc.Contains("Oceanside") || r.inc.Contains("CVS"))).Dump("bad");
	
	references.SelectMany (r => r.r.Elements().Select (x => x.Name.LocalName)).Distinct().Dump("Inner element types");
	
	references
		.Where (r => r.isWebProject)
		.Where (r => (r.LocalName=="ProjectReference" && r.IsCopyLocal!=null && r.IsCopyLocal!="True")
			|| (r.LocalName=="Reference" && r.hintPath!=null && (r.IsCopyLocal==null || r.IsCopyLocal=="False") ))
		.Dump("web projects should be copy local true for non .net refs");
	
	references
		.Where (r => !r.isWebProject)
		.Where (r => r.LocalName=="ProjectReference" && r.IsCopyLocal==null || r.IsCopyLocal=="True")
		.Dump("not optimal");
	
	references.Where (r => r.LocalName=="ProjectReference").Dump("others");