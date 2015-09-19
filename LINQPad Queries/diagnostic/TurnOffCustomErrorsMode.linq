<Query Kind="Statements" />

var configPath=@"\\gtpm-init1-sit\sites$\gtpm-init1\wwwroot\Site\web.config";

if(System.IO.File.Exists(configPath)==false)
{
	"Failed to find".Dump(configPath);
return;
}
var x=System.Xml.Linq.XDocument.Load(configPath);

//x.DumpFormatted();
var systemWeb=x.Root.XPathSelectElement("//system.web");
if(systemWeb==null)
{
	x.DumpFormatted("no system.web");
	return;
}
var ce=systemWeb.XPathSelectElements("customErrors");
if(ce.Count()>1)
{
	ce.Skip(1).Remove();
	x.Save(configPath);
}
if(ce!=null && ce.Any())
{
	var customErrors=ce.First();
	
	customErrors.Attributes(XNamespace.None+"mode").Dump("mode is already");
	ce.Dump("already exists");
	
	return;
}
systemWeb.AddFirst(
	new XElement(systemWeb.Name.Namespace+"customErrors", 
		new XAttribute(XNamespace.None+"mode","Off")));
x.Save(configPath);
x=XDocument.Load(configPath);
x.Root.XPathSelectElement("//system.web/customErrors").Dump("changed to");