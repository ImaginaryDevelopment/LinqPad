<Query Kind="Statements" />

//check deployed case settings
var targetMachine= "vBCDApp1";

//determine case by junction?
var targetcase = "77427";
var customer = "Nova";

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
				let assemblyIdentity=da.Element(n2+"assemblyIdentity").Attribute("name").Value
				let nameShouldContain= customer.IsNullOrEmpty()? targetcase: (customer+"-Cust"+targetcase)
				let name=Util.HighlightIf( assemblyIdentity,a=>a.AfterOrSelf("(").BeforeOrSelf(")").Contains(nameShouldContain)==false)
				select new{deployPath,codebase,name}; //,doc,da};
				
var ad= applicationDependency.First().Dump();

var adQ = from deployment in System.IO.Directory.EnumerateDirectories(System.IO.Path.Combine(deployPath,"Application Files"))
		let versionEnding= System.IO.Path.GetFileName(deployment).AfterLast("_")
		orderby ad.codebase.Contains(System.IO.Path.GetFileName(deployment)) descending, int.Parse(versionEnding) descending
		let configPath=System.IO.Path.Combine(deployment,"Mortgageflex.LoanQuest.exe.config.deploy")
		let config= XDocument.Load(configPath)
		let ns= config.Root.Name.Namespace
		let serviceModeld= (from ssm in config.Root.Elements().Where(ssme=>ssme.Name=="system.serviceModel")
							from ep in ssm.Element("client").Elements("endpoint")
							let shouldTargetAddress=string.Format("http://{0}.mortgageflex.com/{1}/Mortgageflex.Services.Host.LoanQuest/WcfPortal.svc",targetMachine,targetcase)
							let address=Util.HighlightIf( ep.Attribute("address").Value,v=>v.StartsWith(shouldTargetAddress,StringComparison.CurrentCultureIgnoreCase)==false)
							let deployedAs=Util.HighlightIf(deployment.AfterLast("\\"), v=>ad.codebase.Contains(v,StringComparison.CurrentCultureIgnoreCase))
							select new{DeployedAs=deployedAs, Address= address,deployment})
		let appSettings=config.Root.Elements().Where(cre=>cre.Name =="appSettings").Elements().Select(a=>new{Name=a.GetAttribValOrNull("key"),Value=a.GetAttribValOrNull("value")})
		let logging=config.Root.Elements().Where(lc=>lc.Name=="loggingConfiguration").Elements().Where(lc=>lc.Name=="listeners")
			.Elements().Where(lc=>lc.Name=="add" && lc.GetAttribValOrNull("toAddress").IsNullOrEmpty()==false).FirstOrDefault()
		let fromAdd=logging!=null? logging.GetAttribValOrNull("fromAddress"):null
		let toAdd=logging!=null? logging.GetAttribValOrNull("toAddress"):null
	select new{versionEnding,serviceModeld,fromAdd,toAdd,EnvironmentName=appSettings.Where(apps=>apps.Name=="EnvironmentName").Select(a=>a.Value).FirstOrDefault()};
adQ.Take(5).Dump();