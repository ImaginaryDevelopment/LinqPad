<Query Kind="Statements" />

var appConfigPath= @"C:\Microsoft .Net 3.5 Framework\MORTGAGEFLEX PRODUCTS\LoanQuest Origination\Application\LoanQuest\App.config";
//var sandboxAppConfigInfo=System.IO.Directory.EnumerateDirectories( @"\\vBCDApp1\c$\MFWebContent\Cases\77427\LoanQuestNETDeploy\Application Files").Select(d=>new System.IO.DirectoryInfo(d)).OrderByDescending(d=>d.CreationTimeUtc).First();
var serverConfigPath=@"C:\Microsoft .Net 3.5 Framework\MORTGAGEFLEX PRODUCTS\Common Framework\Host\Mortgageflex.Services.Host.LoanQuest\Web.config";
var sandboxServerConfigPath=@"\\vBCDApp1\c$\MFWebContent\Cases\77427\Mortgageflex.Services.Host.LoanQuest\Web.Config";
var xml=System.Xml.Linq.XDocument.Load(appConfigPath);
var rootns= xml.Root.Name.Namespace;
Environment.MachineName.Dump("local machine name");
var aq= from x in xml.Root.Elements(rootns+"system.serviceModel")
		from client in x.Elements(rootns+"client")
		from ep in client.Elements(rootns+"endpoint")
		let name=ep.Attribute("name").Value
		let address= ep.Attribute("address").Value
		select new{name,address=LINQPad.Util.HighlightIf( address,n=>n.Contains(Environment.MachineName, StringComparison.CurrentCultureIgnoreCase))};  //,ep};
	//select x;
		appConfigPath.AsFilePath().ExplorerSelectLink("app.config").Dump();
aq.Dump("app.config");

xml= XDocument.Load(serverConfigPath);
rootns= xml.Root.Name.Namespace;
var sq= from x in xml.Root.Elements(rootns+"connectionStrings")
		from csa in x.Elements(rootns+"add")
		let name=csa.Attribute("name").Value
		let cs= csa.Attribute("connectionString").Value
		let ds= cs.Contains("Data Source=")?cs.After("Data Source=").BeforeOrSelf(";").Trim():null
		let ic= cs.Contains("Initial Catalog=")? cs.After("Initial Catalog=").BeforeOrSelf(";") : null
		orderby name.StartsWith("P") descending, name
		select new{Name=Util.HighlightIf( name,n=>n.Contains("Production")),ds,ic}; //,cs};
		
		serverConfigPath.AsFilePath().ExplorerSelectLink("web.config").Dump();
sq.Dump("server config");
xml=XDocument.Load(sandboxServerConfigPath);
sq= from x in xml.Root.Elements(rootns+"connectionStrings")
		from csa in x.Elements(rootns+"add")
		let name=csa.Attribute("name").Value
		let cs= csa.Attribute("connectionString").Value
		let ds= cs.Contains("Data Source=")?cs.After("Data Source=").BeforeOrSelf(";").Trim():null
		let ic= cs.Contains("Initial Catalog=")? cs.After("Initial Catalog=").BeforeOrSelf(";") : null
		orderby name.StartsWith("P") descending, name
		select new{Name=Util.HighlightIf( name,n=>n.Contains("Production")),ds,ic}; //,cs};
		sq.Dump("sandbox config");


var clientSq= from x in xml.Root.Elements(rootns+"system.serviceModel")
				from client in x.Elements(rootns+"client")
				from ep in client.Elements(rootns+"endpoint")
				let name=ep.Attribute("name").Value
				let address= ep.Attribute("address").Value
				select new{name,address=LINQPad.Util.HighlightIf( address,n=>n.Contains(Environment.MachineName, StringComparison.CurrentCultureIgnoreCase))};  //,ep};
clientSq.Dump("server clients");

var baseDir=@"C:\Microsoft .Net 3.5 Framework\MortgageFlex Products\";
var buildDir=baseDir+@"LoanQuest Origination\bin\release\";
var localHostDir=baseDir+@"Common Framework\HOST\Mortgageflex.Services.Host.LoanQuest\Bin\";

//var toCompare=new[]{"Mortgageflex.Common.dll","Mortgageflex.LoanQuest.Library.Registration.dll","Mortgageflex.LoanQuest.Library.Setup.dll"};
var sandboxDir=@"\\vBCDApp1\c$\MFWebContent\Cases\77427\Mortgageflex.Services.Host.LoanQuest\Bin";
var copyFileText=@"	var source= @""{0}"";
	var dest=@""{1}"";
	System.IO.File.Copy(source,dest,true);
	System.IO.Path.GetFileName(source).Dump(""copied to ""+System.IO.Path.GetDirectoryName(dest));
	source= source.Before("".dll"")+"".pdb"";
	dest= dest.Before("".dll"")+"".pdb"";
	if(System.IO.File.Exists(source))
		System.IO.File.Copy(source,dest,true);";
var hostVsBuildQ = from i in System.IO.Directory.EnumerateFiles(buildDir,"*.dll")
	let fileName=System.IO.Path.GetFileName(i)
	let hostPath= System.IO.Path.Combine(localHostDir, fileName)
	where System.IO.File.Exists(hostPath)
	let sandBoxPath= System.IO.Path.Combine(sandboxDir,fileName)
	let buildInfo=new System.IO.FileInfo(i)
	let hostInfo= new System.IO.FileInfo(hostPath)
	let sandBoxInfo = new System.IO.FileInfo(sandBoxPath)
	let creation=new{ Built= buildInfo.CreationTimeUtc,Hosted= hostInfo.CreationTimeUtc,SandBox=sandBoxInfo.CreationTimeUtc}
	let size= new{ Built=buildInfo.Length, LocalHost=hostInfo.Length,SandBox=sandBoxInfo.Length}
	let versions= new{Built= FileVersionInfo.GetVersionInfo(i), Hosted= FileVersionInfo.GetVersionInfo(hostPath),SandBox=FileVersionInfo.GetVersionInfo(sandBoxPath)}
	let fileVersion =new{Built= versions.Built.FileVersion, Hosted= versions.Hosted.FileVersion,SandBox=versions.SandBox.FileVersion}
	let productVersion= new{Built= versions.Built.ProductVersion, Hosted= versions.Hosted.ProductVersion,SandBox=versions.SandBox.ProductVersion}
	where size.Built!= size.LocalHost || size.Built!=size.SandBox
	
	select new{Item=new Hyperlinq( QueryLanguage.Statements,string.Format(copyFileText,hostInfo.FullName,sandBoxInfo.FullName), buildInfo.Name), //LINQPad.Util.HighlightIf(i,_=>buildInfo.CreationTimeUtc!=hostInfo.CreationTimeUtc || buildInfo.Length!=hostInfo.Length),
		Creation= Util.HighlightIf(creation,a=>a.Built!=a.Hosted),
		FileVersion =Util.HighlightIf(fileVersion ,a=>a.Built!=a.Hosted),
		ProductVersion= Util.HighlightIf(productVersion ,a=>a.Built!=a.Hosted),
		Size=Util.HighlightIf(size,a=>a.Built!=a.LocalHost || a.Built!= a.SandBox)
		};
	
hostVsBuildQ.Dump("buildVsHostMismatches");