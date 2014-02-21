<Query Kind="Statements" />

var servers=System.Environment.GetEnvironmentVariable("servers", EnvironmentVariableTarget.User).Split(';');
var configBase=@"\\"+servers[0]+@"\c$\inetpub\";
var directories=System.IO.Directory.GetDirectories(configBase).Where (d =>System.IO.File.Exists( System.IO.Path.Combine(d,"web.config"))).Select (d => d.AfterLastOrSelf("\\")).ToArray();
var site=Util.ReadLine("site?",string.Empty,directories.Dump("site options"));
var configPath=configBase+site+@"\Web.config";
configPath.Dump("adjusting at");
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