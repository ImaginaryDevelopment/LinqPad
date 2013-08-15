<Query Kind="Program" />

void Main()
{
	var csprojPath=@"C:\Microsoft .Net 3.5 Framework\Mortgageflex products\Common Framework\Mortgageflex.Scripting\Mortgageflex.Scripting.csproj";
	csprojPath=Util.ReadLine("csproj?",csprojPath);
	Debug.Assert(csprojPath.EndsWith(".csproj"),"invalid csproj file");
	Debug.Assert(System.IO.File.Exists( csprojPath),"could not find csproj file");
	var commonPath=@"C:\Microsoft .Net 3.5 Framework\Mortgageflex products\Common Framework\bin\debug\";
	var commonTransportPath=@"C:\Microsoft .Net 3.5 Framework\Mortgageflex products\LoanQuest Transport Server\bin\debug\";
	var thirdRefPath=@"C:\Microsoft .Net 3.5 Framework\references\";
	
	Debug.Assert(System.IO.Directory.Exists(commonPath),"Common Path not found");
	Debug.Assert(System.IO.Directory.Exists(thirdRefPath),"Third Party references path not found");
	
	var csprojDirPath=System.IO.Path.GetDirectoryName(csprojPath);
	if(csprojDirPath.EndsWith("\\")==false)
		csprojDirPath+="\\";
	Debug.Assert(csprojDirPath.EndsWith("\\"),"Directory ending is incorrect for relativity");
	
	var commonRelativePath =MakeRelative(csprojDirPath,commonPath).Replace("debug","$(Configuration)");
	var commonTransportRelativePath=MakeRelative(csprojDirPath,commonTransportPath).Replace("debug","$(Configuration)");
	var thirdRelativePath = MakeRelative(csprojDirPath,thirdRefPath);
	
	new XElement("ThirdPartyReferencePath",thirdRelativePath).ToString().Dump();
	new XElement("CommonFrameworkReferencesPath",commonRelativePath).ToString().Dump();
	new XElement("CommonTransportReferencesPath",commonTransportRelativePath).ToString().Dump();
	
	var thirdPartyHintPath=new XElement("HintPath", "$(ThirdPartyReferencePath).dll");
	thirdPartyHintPath.ToString().Dump();
	var commonHintPath = new XElement("HintPath", "$(CommonFrameworkReferencesPath).dll");
	commonHintPath.ToString().Dump();
	var transportHintPath= new XElement("HintPath","$(CommonTransportReferencesPath).dll");
	transportHintPath.ToString().Dump();
	
	var csproj=XDocument.Load(csprojPath);
	var ns= csproj.Root.Name.Namespace;
	var q= from igs in csproj.Root.Elements(ns+"ItemGroup")
			from r in igs.Elements(ns+"Reference")
			let include=r.GetAttribValOrNull("Include")
			let specific= r.Element(ns+"SpecificVersion")
			let isPrivate=r.Element(ns+"Private")
			where include.StartsWith("System")==false && include.StartsWith("mscorlib")==false
			let desiredProvider=include.StartsWith("Mortgage")==false?thirdPartyHintPath:
				include.Contains("Transport")? transportHintPath :
				 commonHintPath
			let fullPath= desiredProvider.Value
				.Replace("$(ThirdPartyReferencePath)",thirdRefPath)
				.Replace("$(CommonFrameworkReferencesPath)",commonPath)
				.Replace("$(CommonTransportReferencesPath)",commonTransportPath)
				.Replace(".dll",include.BeforeOrSelf(",")+".dll")
			let desiredHintPath= desiredProvider.Value
			
			//select new{ r,include,specific};
			select new{Name=include.BeforeOrSelf(","),
			FullPath=Util.HighlightIf(fullPath,f=>System.IO.File.Exists(f)==false),
			Node=
				new XElement(r.Name.LocalName,
				new XAttribute("Include",include),
				new XElement("HintPath",
				desiredHintPath.Replace(".dll",include.BeforeOrSelf(",")+".dll")),
				specific!=null?new XElement(specific.Name.LocalName,specific.Value):null,
				isPrivate!=null?new XElement(isPrivate.Name.LocalName,isPrivate.Value):null
				),
				Old=r};
			
			//q.Dump();
			q.Select(a=>a.Node).Dump();
		
}

// Define other methods and classes here
string MakeRelative( string pathtarget,string pathSource){
	var targetUri= new Uri(pathtarget);
	var relativePath =targetUri.MakeRelativeUri(new Uri(pathSource)).ToString().Replace("/","\\").Replace("%20"," ");
	return relativePath;
}