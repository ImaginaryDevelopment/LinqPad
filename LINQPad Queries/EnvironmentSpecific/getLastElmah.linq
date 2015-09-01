<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.XML.dll</Reference>
</Query>

void Main()
{
	var elmahDir=@"\\crprdnii1h9\sites$\Gtpm-init1\wwwroot\Site\App_Data";
	var files=GetFiles(elmahDir);
	
	var doc=files.First();
	doc.DumpFormatted();
	
}

// Define other methods and classes here
IEnumerable<XDocument> GetFiles(string path)
{
	foreach(var filePath in System.IO.Directory.GetFiles(path).OrderByDescending(f=>System.IO.File.GetLastWriteTimeUtc(path)))
	
	yield return System.Xml.Linq.XDocument.Load(filePath);
}