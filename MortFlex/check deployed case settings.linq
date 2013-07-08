<Query Kind="Statements" />

//check deployed case settings
var targetMachine= "vBCDApp1";
var targetcase = "77427";
var deployPath=string.Format("\\\\{0}\\C$\\MFWebContent\\Cases\\{1}\\LoanQuestNETDeploy",targetMachine,targetcase);
var applicationFullPath = System.IO.Path.Combine(deployPath,"MortgageFlex.LoanQuest.application");
var appDoc=XDocument.Load(applicationFullPath);
var applicationDependency = 
				from doc in  appDoc.Root.Elements()
				let ns= appDoc.Root.Name.Namespace
				let n2= XNamespace.Get("urn:schemas-microsoft-com:asm.v2")
				where doc.Name==n2+"dependency"
				from da in doc.Elements(n2+"dependentAssembly")
				let codebase= da.Attribute("codebase").Value
				let name= da.Element(n2+"assemblyIdentity").Attribute("name").Value
				select new{deployPath,codebase,name}; //,doc,da};
				
				
var ad= applicationDependency.First().Dump();
var adQ = from deployment in System.IO.Directory.EnumerateDirectories(System.IO.Path.Combine(deployPath,"Application Files"))
		let versionEnding= System.IO.Path.GetFileName(deployment).AfterLast("_")
		orderby ad.codebase.Contains(System.IO.Path.GetFileName(deployment)) descending, int.Parse(versionEnding) descending
		let configPath=System.IO.Path.Combine(deployment,"Mortgageflex.LoanQuest.exe.config.deploy")
		let config= XDocument.Load(configPath)
		from e in config.Root.Elements()
		let ns= config.Root.Name.Namespace
	where e.Name=="system.serviceModel"
	from ep in e.Element("client").Elements("endpoint")
	let address=Util.HighlightIf( ep.Attribute("address").Value,v=>v.Contains(targetMachine,StringComparison.CurrentCultureIgnoreCase)==false)
	let deployedAs=Util.HighlightIf(deployment.AfterLast("\\"), v=>ad.codebase.Contains(v,StringComparison.CurrentCultureIgnoreCase))
	select new{DeployedAs=deployedAs, Address= address,deployment};
adQ.Dump();
