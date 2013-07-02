<Query Kind="Statements" />

var appConfigPath= @"C:\Microsoft .Net 3.5 Framework\MORTGAGEFLEX PRODUCTS\LoanQuest Origination\Application\LoanQuest\App.config";
var serverConfigPath=@"C:\Microsoft .Net 3.5 Framework\MORTGAGEFLEX PRODUCTS\Common Framework\Host\Mortgageflex.Services.Host.LoanQuest\Web.config";
var xml=System.Xml.Linq.XDocument.Load(appConfigPath);
var rootns= xml.Root.Name.Namespace;
Environment.MachineName.Dump("machine name");
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
var clientSq= from x in xml.Root.Elements(rootns+"system.serviceModel")
				from client in x.Elements(rootns+"client")
				from ep in client.Elements(rootns+"endpoint")
				let name=ep.Attribute("name").Value
				let address= ep.Attribute("address").Value
				select new{name,address=LINQPad.Util.HighlightIf( address,n=>n.Contains(Environment.MachineName, StringComparison.CurrentCultureIgnoreCase))};  //,ep};
clientSq.Dump("server clients");