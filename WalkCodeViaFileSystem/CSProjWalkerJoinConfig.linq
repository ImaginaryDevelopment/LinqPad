<Query Kind="Program" />

void Main()
{
	//bool debug=false;
	var baseDir=Util.ReadLine("Directory?",System.Environment.GetEnvironmentVariable("devroot"));
	
	var projects= System.IO.Directory.GetFiles(baseDir,"*.*proj", SearchOption.AllDirectories);
	//sample projFile
	//XDocument.Load(projects.Take(1).Single ( )).DumpFormatted(projects.Take(1).Single ());
	var csProjects=projects.Where(f=>f.EndsWith(".csproj") && f.EndsWith("WordsMatching.csproj")==false && f.Contains("test", StringComparison.InvariantCultureIgnoreCase)==false);//.Take(2);
	var baseQuery=(from i in csProjects
		let doc=XDocument.Load(i)
		let rootns=doc.Root.Name.Namespace
		let proj=doc.Element(rootns+"Project").DumpIf(x=>x==null,i+" has no project element")
		where proj!=null
		let csproj= new CsProj(doc.Root)
		
		select new{ Path=i, Doc=doc,csproj}).ToArray();
		
		
		
		
		
		
	var projsWithConfig= from i in baseQuery
		let config= i.csproj.GetItems().FirstOrDefault (c =>c.GetAttribValOrNull("Include")!=null && c.GetAttribValOrNull("Include").IsIgnoreCaseMatch("app.config")|| c.GetAttribValOrNull("Include").IsIgnoreCaseMatch("web.config"))
		where config!=null
		let configPath = i.Path.AsFilePath().GetDirectoryName().AsDirPath().PathCombine(config.GetAttribValOrNull("Include"))
		let configDoc = XDocument.Load(configPath)
		let configFile= new ConfigurationFile(configDoc.Root)
		where configFile.GetRuntime()!=null
		select new{Project=i.Path,configPath,ab=configFile.GetAssemblyBindings(), References= i.csproj.GetFileReferences(),configInclude=config};

		projsWithConfig.Take(5).Dump("all references",3);
		baseQuery.Take(4).Dump("proj nodes",1);
		
		//find project's config and join it, if it exists
//	var runtime=from i in baseQuery
//		
//		from r in i.ProjNode.Elements(i.RootNs+"runtime")
//		where r!=null
//		let abNs=r.GetNamespaceOfPrefix("urn")
//		let ab=r.Element(abNs+"assemblyBinding")
//		select new{r,ab};
//		runtime.Dump("runtime",1);
	
}

public class ConfigurationFile{
	readonly XElement _configuration;
	readonly XNamespace _rootns;
	Lazy<XElement> _runtime;
	public ConfigurationFile(XElement configuration){
		if(configuration.Name.LocalName!="configuration")
			throw new ArgumentOutOfRangeException();
		_configuration=configuration;
		_rootns=configuration.Name.Namespace;
		_runtime= new Lazy<XElement>(()=>_configuration.Element(_rootns+"runtime"));
	}
	public XElement GetRuntime(){
		return _runtime.Value;
	}
	public IEnumerable<XElement> GetAssemblyBindings(){
		var runtime=GetRuntime();
		if(runtime==null)
			return null;
		return runtime.Elements().Where (r => r.Name.LocalName=="assemblyBinding");
	}
	public override string ToString(){
		return _configuration.ToString();
	}
}
// Define other methods and classes here
public class CsProj {
	readonly XElement _proj ;
	XNamespace _rootns;
	Lazy<IEnumerable<XElement>> _ItemGroups;
	Lazy<IEnumerable<XElement>> _PropertyGroups;
	
	public CsProj(XElement proj){
		if(proj.Name.LocalName!="Project")
			throw new ArgumentOutOfRangeException();
		_proj=proj;
		_rootns=proj.Name.Namespace;
		_ItemGroups=new Lazy<IEnumerable<XElement>>(()=>_proj.Elements(_rootns+"ItemGroup"));
		//_proj.Elements().Dump("projElements",1);
		_PropertyGroups = new Lazy<IEnumerable<XElement>>(()=>_proj.Elements(_rootns+"PropertyGroup"));
	}
	
	public IEnumerable<XElement> GetItemGroups(){
		return _ItemGroups.Value;
	}
	public IEnumerable<XElement> GetItems(){
		return _ItemGroups.Value.SelectMany (v => v.Elements());
	}
	public IEnumerable<XElement> GetPropertyGroups(){
		return _PropertyGroups.Value ;
	}
	public IEnumerable<XElement> GetReferences(){
		return GetItemGroups().Take(2).SelectMany (ig => ig.Elements().Where (i => i.Name.LocalName=="Reference" || i.Name.LocalName=="ProjectReference"));
	}
	public IEnumerable<XElement> GetFileReferences(){
		return GetItemGroups().SelectMany (ig => ig.Elements(_rootns+"Reference"));
	}
	public IEnumerable<XElement> GetProjectReferences(){
		return GetReferences().Where (r => r.Name.LocalName=="ProjectReference");
	}
	public override string ToString(){
		return _proj.ToString();
	}
}