<Query Kind="Statements" />

var configPath=@"\\SVRRBIDEV03\c$\inetpub\dev.clearvoiceresearch.com\Web.config";

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