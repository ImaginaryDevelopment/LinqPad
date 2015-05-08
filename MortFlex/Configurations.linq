<Query Kind="Program" />

void Main()
{
	
	var caseNum="77783";
	var customerDefault="Homestreet";
	var sandboxDefault= "vBcdApp1";
	var mode = Util.ReadLine("Debug or release?","debug",new[]{"debug","release"});
	var targetCase= Util.ReadLine("Target case?",caseNum);
	var customer=Util.ReadLine("Customer?", customerDefault	,new[]{ "Homestreet","Nova"});
	var baseDir=@"C:\Microsoft .Net 3.5 Framework\";
	var junctionDir=baseDir+@"MortgageFlex Products\";
	var buildDir=junctionDir+@"LoanQuest Origination\bin\"+mode+"\\";
	
	var localhostBase =junctionDir+@"Common Framework\HOST\";
	var localHostDir=localhostBase+@"Mortgageflex.Services.Host.LoanQuest\Bin\";
	var sandboxBase = String.Format(@"\\{0}\c$\MFWebContent\Cases\{1}\", sandboxDefault,targetCase);
	
	var sandboxDir=String.Format(sandboxBase+@"Mortgageflex.Services.Host.LoanQuest\Bin", sandboxDefault,targetCase);
	var sandboxCase=sandboxDir.After("Cases\\").Before("\\");
	var junction= new DirectoryPathWrapper( baseDir).GetJunctions().First();
	string junctionTargetPath=junction.Item2;
	string junctionCase=junctionTargetPath.After("_").Dump("junction case");
	CheckDeployedCase(targetCase,customer,sandboxDefault);
	var sandboxMatchesJunction= sandboxDir.Contains(junctionCase) || sandboxDir.Contains(targetCase);
	if(sandboxMatchesJunction)
	{
		//CompareBin(buildDir,localHostDir,sandboxDir);
		CompareBins(buildDir,localhostBase,sandboxBase,@"Mortgageflex.Services.Host.LoanQuest\Bin",@"Mortgageflex.Services.Host.PrintingService\Bin");
	}	else {
		new{ SandboxCase= sandboxCase,JunctionCase=junctionCase}.Dump();
		Util.Highlight("junction not lined up with sandbox, aborting bin compare").Dump();
	}
	
	var appConfigPath=junctionDir+ @"LoanQuest Origination\Application\LoanQuest\App.config";
	//var sandboxAppConfigInfo=System.IO.Directory.EnumerateDirectories( @"\\vBCDApp1\c$\MFWebContent\Cases\77427\LoanQuestNETDeploy\Application Files").Select(d=>new System.IO.DirectoryInfo(d)).OrderByDescending(d=>d.CreationTimeUtc).First();
	var serverConfigPath=junctionDir+@"Common Framework\Host\Mortgageflex.Services.Host.LoanQuest\Web.config";
	var regServerConfigPath=junctionDir+@"Common Framework\Host\Mortgageflex.Services.Host.LoanQuest\Web.config";
	var sandboxServerConfigPath=String.Format( @"\\{0}\c$\MFWebContent\Cases\{1}\Mortgageflex.Services.Host.LoanQuest\Web.Config",sandboxDefault,targetCase);
	var sandboxRegConfigPath = String.Format( @"\\{0}\c$\MFWebContent\Cases\{1}\Mortgageflex.Services.Host.Registration\Web.Config",sandboxDefault,targetCase);
	Environment.MachineName.Dump("local machine name");
	ShowAppConfig("localHost",appConfigPath);
	
	var serverMappings = new Dictionary<string,string>(){
		{"local:"+junctionCase+"("+targetCase+")",serverConfigPath},
		{"local Registration:"+sandboxCase,regServerConfigPath},
		{"sandbox:"+sandboxCase,sandboxServerConfigPath},
		{"sandbox registration:"+sandboxCase,sandboxRegConfigPath}
	};
	foreach(var sm in serverMappings){
		if(System.IO.File.Exists(sm.Value)){
			ShowServerConfig(sm.Key,sm.Value);
		} else {
			sm.Value.Dump("not found, unable to show server config");
		}
	}
}

void CheckDeployedCase(string targetCase,string customer,string sandbox){
//check deployed case settings
var targetMachine= sandbox;


var deployPath=string.Format("\\\\{0}\\C$\\MFWebContent\\Cases\\{1}\\LoanQuestNETDeploy",targetMachine,targetCase);
var applicationFullPath = System.IO.Path.Combine(deployPath,"MortgageFlex.LoanQuest.application");
if(System.IO.File.Exists(applicationFullPath)==false)
{
	applicationFullPath.Dump( "could not find file");
	return;
}
var appDoc=XDocument.Load(applicationFullPath);
var applicationDependency = 
				from doc in  appDoc.Root.Elements()
				let ns= appDoc.Root.Name.Namespace
				let n2= XNamespace.Get("urn:schemas-microsoft-com:asm.v2")
				where doc.Name==n2+"dependency"
				from da in doc.Elements(n2+"dependentAssembly")
				let codebase= da.Attribute("codebase").Value
				let assemblyIdentity=da.Element(n2+"assemblyIdentity").Attribute("name").Value
				let nameShouldContain= customer.IsNullOrEmpty()? targetCase: (customer+"-Cust"+targetCase)
				let name=Util.HighlightIf( assemblyIdentity,a=>a.AfterOrSelf("(").BeforeOrSelf(")").Contains(nameShouldContain)==false)
				select new{deployPath,codebase,name}; //,doc,da};
				
var ad= applicationDependency.First().Dump();

var adQ = from deployment in System.IO.Directory.EnumerateDirectories(System.IO.Path.Combine(deployPath,"Application Files"))
		let versionEnding= System.IO.Path.GetFileName(deployment).AfterLast("_")
		let deploymentDt= new System.IO.DirectoryInfo(deployment).CreationTime
		orderby ad.codebase.Contains(System.IO.Path.GetFileName(deployment)) descending, int.Parse(versionEnding) descending
		let configPath=System.IO.Path.Combine(deployment,"Mortgageflex.LoanQuest.exe.config.deploy")
		let config= XDocument.Load(configPath)
		let ns= config.Root.Name.Namespace
		let serviceModeld= (from ssm in config.Root.Elements().Where(ssme=>ssme.Name=="system.serviceModel")
							from ep in ssm.Element("client").Elements("endpoint")
							let shouldTargetAddress=string.Format("http://{0}.mortgageflex.com/{1}/Mortgageflex.Services.Host.LoanQuest/WcfPortal.svc",targetMachine,targetCase)
							let address=Util.HighlightIf( ep.Attribute("address").Value,v=>v.StartsWith(shouldTargetAddress,StringComparison.CurrentCultureIgnoreCase)==false)
							let deployedAs=Util.HighlightIf(deployment.AfterLast("\\"), v=>ad.codebase.Contains(v,StringComparison.CurrentCultureIgnoreCase))
							select new{DeployedAs=deployedAs, Address= address,deployment})
		let appSettings=config.Root.Elements().Where(cre=>cre.Name =="appSettings").Elements().Select(a=>new{Name=a.GetAttribValOrNull("key"),Value=a.GetAttribValOrNull("value")})
		let logging=config.Root.Elements().Where(lc=>lc.Name=="loggingConfiguration").Elements().Where(lc=>lc.Name=="listeners")
			.Elements().Where(lc=>lc.Name=="add" && lc.GetAttribValOrNull("toAddress").IsNullOrEmpty()==false).FirstOrDefault()
		let fromAdd=logging!=null? logging.GetAttribValOrNull("fromAddress"):null
		let toAdd=logging!=null? logging.GetAttribValOrNull("toAddress"):null
	select new{versionEnding,deploymentDt,serviceModeld,fromAdd,toAdd,EnvironmentName=appSettings.Where(apps=>apps.Name=="EnvironmentName").Select(a=>a.Value).FirstOrDefault()};
adQ.Take(5).Dump();
}

void CompareBins(string built, string localhostBase, string sandboxBase,params string[] targets){
	foreach(var t in targets){
	if(System.IO.Directory.Exists(sandboxBase+t))
		{
			
			CompareBin(built,localhostBase+t,sandboxBase+t,t+":");
		}
	}
}
Assembly TryReflectionLoad(string path){
	try
	{	        
		return System.Reflection.Assembly.ReflectionOnlyLoadFrom(path);
	}
	catch (Exception ex)
	{
		
		ex.Dump();
		return null;
	}
}
void CompareBin(string builtDir, string localhostDir,string sandboxDir,string description=null){

var copyFileText=@"	var source= @""{0}"";
		var dest=@""{1}"";
		System.IO.File.Copy(source,dest,true);
		source.Dump(""copied to ""+System.IO.Path.GetDirectoryName(dest));
		source= source.Before("".dll"")+"".pdb"";
		dest= dest.Before("".dll"")+"".pdb"";
		if(System.IO.File.Exists(source)){{
			System.IO.File.Copy(source,dest,true);
			source.Dump(""copied to ""+System.IO.Path.GetDirectoryName(dest));	
		}}";
	var hostVsBuildQ = from i in System.IO.Directory.EnumerateFiles(builtDir,"*.dll")
		let fileName=System.IO.Path.GetFileName(i)
		let hostPath= System.IO.Path.Combine(localhostDir, fileName)
		where System.IO.File.Exists(hostPath)
		let sandBoxPath= System.IO.Path.Combine(sandboxDir,fileName)
		where System.IO.File.Exists(sandBoxPath)
		let buildInfo=new System.IO.FileInfo(i)
		let hostInfo= new System.IO.FileInfo(hostPath)
		let sandBoxInfo = new System.IO.FileInfo(sandBoxPath)
		let builtAssInfo = TryReflectionLoad(i)
		//let runtime= new{ Built= builtAssInfo}
		let creation=new{ Built= buildInfo.CreationTime,Hosted= hostInfo.CreationTime,SandBox=sandBoxInfo.CreationTime}
		let modification= new {Built= buildInfo.LastWriteTime,Hosted= hostInfo.LastWriteTime, SandBox= sandBoxInfo.LastWriteTime}
		let size= new{ Built=buildInfo.Length, LocalHost=hostInfo.Length,SandBox=sandBoxInfo.Length}
		let versions= new{Built= FileVersionInfo.GetVersionInfo(i), Hosted= FileVersionInfo.GetVersionInfo(hostPath),SandBox=FileVersionInfo.GetVersionInfo(sandBoxPath)}
		let fileVersion =new{Built= versions.Built.FileVersion, Hosted= versions.Hosted.FileVersion,SandBox=versions.SandBox.FileVersion}
		let productVersion= new{Built= versions.Built.ProductVersion, Hosted= versions.Hosted.ProductVersion,SandBox=versions.SandBox.ProductVersion}
		let deployDifferenceMinutes=Math.Round( Math.Abs(( modification.SandBox-modification.Built).TotalMinutes))
		where size.Built!= size.LocalHost || size.Built!=size.SandBox || (modification.Built!= modification.SandBox && deployDifferenceMinutes>3) //at least x min apart
		orderby fileName.StartsWith("Mort") descending,buildInfo.LastWriteTimeUtc descending, fileName
		select new{Item=new Hyperlinq( QueryLanguage.Statements,string.Format(copyFileText,buildInfo.FullName,sandBoxInfo.FullName), buildInfo.Name), //LINQPad.Util.HighlightIf(i,_=>buildInfo.CreationTimeUtc!=hostInfo.CreationTimeUtc || buildInfo.Length!=hostInfo.Length),
			Modification=Util.HighlightIf(modification,a=>a.Built!=a.SandBox),
			//ModDifference= deployDifferenceMinutes,
			//runtime,
			FileVersion =Util.HighlightIf(fileVersion ,a=>a.Built!=a.Hosted),
			ProductVersion= Util.HighlightIf(productVersion ,a=>a.Built!=a.Hosted),
			Size=Util.HighlightIf(size,a=>a.Built!=a.LocalHost || a.Built!= a.SandBox),
			Creation= creation //Util.HighlightIf(creation,a=>a.Built!=a.Hosted),
			};
		
	hostVsBuildQ.Dump(description+"buildVsHostMismatches");
}
// Define other methods and classes here
void ShowServerConfig(string description, string path){
	var xml=System.Xml.Linq.XDocument.Load(path);
	var rootns= xml.Root.Name.Namespace;
	var sq= from x in xml.Root.Elements(rootns+"connectionStrings")
			from csa in x.Elements(rootns+"add")
			let name=csa.Attribute("name").Value
			let cs= csa.Attribute("connectionString").Value
			let ds= cs.Contains("Data Source=")?cs.After("Data Source=").BeforeOrSelf(";").Trim():null
			let ic= cs.Contains("Initial Catalog=")? cs.After("Initial Catalog=").BeforeOrSelf(";") : null
			orderby name.StartsWith("P") descending, name
			select new{Name=Util.HighlightIf( name,n=>n.Contains("Production")),ds,ic}; //,cs};
			
			path.AsFilePath().AsExplorerSelectLink("web.config").Dump(description);
	sq.Dump("server config:"+description);
	
	var clientSq= from x in xml.Root.Elements(rootns+"system.serviceModel")
					from client in x.Elements(rootns+"client")
					from ep in client.Elements(rootns+"endpoint")
					let name=ep.Attribute("name").Value
					let address= ep.Attribute("address").Value
					select new{name,address=LINQPad.Util.HighlightIf( address,n=>n.Contains(Environment.MachineName, StringComparison.CurrentCultureIgnoreCase))};  //,ep};
	clientSq.Dump(description+" server clients");
}

void ShowAppConfig(string description, string path){

	var xml=System.Xml.Linq.XDocument.Load(path);
	var rootns= xml.Root.Name.Namespace;
	
	var aq= from x in xml.Root.Elements(rootns+"system.serviceModel")
			from client in x.Elements(rootns+"client")
			from ep in client.Elements(rootns+"endpoint")
			let name=ep.Attribute("name").Value
			let address= ep.Attribute("address").Value
			select new{name,address=LINQPad.Util.HighlightIf( address,n=>n.Contains(Environment.MachineName, StringComparison.CurrentCultureIgnoreCase))};  //,ep};
		//select x;
			path.AsFilePath().AsExplorerSelectLink("app.config").Dump();
	aq.Dump("app.config");
}