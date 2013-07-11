<Query Kind="Program" />

void Main()
{
var baseDir=@"C:\Microsoft .Net 3.5 Framework\MortgageFlex Products\";
	var buildDir=baseDir+@"LoanQuest Origination\bin\release\";
	var localHostDir=baseDir+@"Common Framework\HOST\Mortgageflex.Services.Host.LoanQuest\Bin\";
	
	var sandboxDir=@"\\vBCDApp1\c$\MFWebContent\Cases\77427\Mortgageflex.Services.Host.LoanQuest\Bin";
	CompareBin(buildDir,localHostDir,sandboxDir);
	
	var appConfigPath= @"C:\Microsoft .Net 3.5 Framework\MORTGAGEFLEX PRODUCTS\LoanQuest Origination\Application\LoanQuest\App.config";
	//var sandboxAppConfigInfo=System.IO.Directory.EnumerateDirectories( @"\\vBCDApp1\c$\MFWebContent\Cases\77427\LoanQuestNETDeploy\Application Files").Select(d=>new System.IO.DirectoryInfo(d)).OrderByDescending(d=>d.CreationTimeUtc).First();
	var serverConfigPath=@"C:\Microsoft .Net 3.5 Framework\MORTGAGEFLEX PRODUCTS\Common Framework\Host\Mortgageflex.Services.Host.LoanQuest\Web.config";
	var sandboxServerConfigPath=@"\\vBCDApp1\c$\MFWebContent\Cases\77427\Mortgageflex.Services.Host.LoanQuest\Web.Config";
	Environment.MachineName.Dump("local machine name");
	ShowAppConfig("localHost",appConfigPath);
	
	ShowServerConfig("local",serverConfigPath);
	ShowServerConfig("sandbox",sandboxServerConfigPath);
	
	
}

void CompareBin(string builtDir, string localhostDir,string sandboxDir){

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
		let buildInfo=new System.IO.FileInfo(i)
		let hostInfo= new System.IO.FileInfo(hostPath)
		let sandBoxInfo = new System.IO.FileInfo(sandBoxPath)
		let creation=new{ Built= buildInfo.CreationTime,Hosted= hostInfo.CreationTime,SandBox=sandBoxInfo.CreationTime}
		let modification= new {Built= buildInfo.LastWriteTime,Hosted= hostInfo.LastWriteTime, SandBox= sandBoxInfo.LastWriteTime}
		let size= new{ Built=buildInfo.Length, LocalHost=hostInfo.Length,SandBox=sandBoxInfo.Length}
		let versions= new{Built= FileVersionInfo.GetVersionInfo(i), Hosted= FileVersionInfo.GetVersionInfo(hostPath),SandBox=FileVersionInfo.GetVersionInfo(sandBoxPath)}
		let fileVersion =new{Built= versions.Built.FileVersion, Hosted= versions.Hosted.FileVersion,SandBox=versions.SandBox.FileVersion}
		let productVersion= new{Built= versions.Built.ProductVersion, Hosted= versions.Hosted.ProductVersion,SandBox=versions.SandBox.ProductVersion}
		let deployDifferenceMinutes=Math.Round( Math.Abs(( modification.SandBox-modification.Built).TotalMinutes))
		where size.Built!= size.LocalHost || size.Built!=size.SandBox || (modification.Built!= modification.SandBox && deployDifferenceMinutes>3) //at least x min apart
		orderby fileName.StartsWith("Mort") descending,buildInfo.CreationTimeUtc descending, fileName
		select new{Item=new Hyperlinq( QueryLanguage.Statements,string.Format(copyFileText,buildInfo.FullName,sandBoxInfo.FullName), buildInfo.Name), //LINQPad.Util.HighlightIf(i,_=>buildInfo.CreationTimeUtc!=hostInfo.CreationTimeUtc || buildInfo.Length!=hostInfo.Length),
			Modification=Util.HighlightIf(modification,a=>a.Built!=a.SandBox),
			//ModDifference= deployDifferenceMinutes,
			FileVersion =Util.HighlightIf(fileVersion ,a=>a.Built!=a.Hosted),
			ProductVersion= Util.HighlightIf(productVersion ,a=>a.Built!=a.Hosted),
			Size=Util.HighlightIf(size,a=>a.Built!=a.LocalHost || a.Built!= a.SandBox),
			Creation= creation //Util.HighlightIf(creation,a=>a.Built!=a.Hosted),
			};
		
	hostVsBuildQ.Dump("buildVsHostMismatches");
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