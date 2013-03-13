<Query Kind="Program">
  <Namespace>System.Xml.Schema</Namespace>
</Query>

void Main()
{

	var environments=new[] { 
		@"\\gtpm-init1-dit\sites\gtpm-init1\" ,
		@"\\gtpm-init1-sit\sites$\Gtpm-init1\",
		@"\\gtpm-init1-dit\sites\gtpm-bau1\" 
		};
	
	
	var transforms=new List<string>();
	var webConfigs=new List<GtpmConfigurationValidator>();
	foreach(var env in environments)
	foreach(var file in RecurseLocation(env,env,new[]{"*.config"}))
	{
	if(file.EndsWith("debug.config",StringComparison.CurrentCultureIgnoreCase))
		transforms.Add(file);
		    try
			{	        
				var doc=System.Xml.Linq.XDocument.Load(file, LoadOptions.None);
	
		switch(doc.Root.Name.ToString())
	{
	case "configuration":
			{
			var config=new GtpmConfigurationValidator(file,doc);
			if(!config.IsValid())
			file.Dump("did not validate");
			webConfigs.Add(config);
			}
	break;
	default:
	break;
	}
		
	
			}
			catch (XmlException ex)
			{
				ex.Dump(file);
				
			} 
		
	
	}
	transforms.Dump("transform config found deployed");
}

public class GtpmConfigurationValidator
{
public string Path{get;private set;}
readonly XDocument _doc;
	public GtpmConfigurationValidator(string path,XDocument doc)
	{
		Path=path;
		_doc=doc;
	}
	public bool IsValid()
	{
		return true;
	}
	
}

public class FileSummary
{
	public string RelativePath{get;set;}
	public string Path{get;set;}
}
// Define other methods and classes here
//static XmlReader schemaReader =XmlReader.Create(@"d:\program files (x86)\Microsoft Visual Studio 10.0\xml\Schemas\1033\MSBuild\Microsoft.Build.CommonTypes.xsd");
//public void VerifyXmlFile(string path)
//{
//	// configure the xmlreader validation to use inline schema.
//	XmlReaderSettings config = new XmlReaderSettings();
//	schemaReader.BaseURI.Dump();
//	schemaReader.NamespaceURI.Dump("reader namespace");
//	"test".Dump();
//	//config.Schemas.Add(schemaReader.NamespaceURI,schemaReader);
//	
//	config.ValidationType = ValidationType.Schema;
//	config.ValidationFlags |= XmlSchemaValidationFlags.ReportValidationWarnings;
//	config.ValidationFlags |= XmlSchemaValidationFlags.ProcessInlineSchema;
//	config.ValidationFlags |= XmlSchemaValidationFlags.ProcessSchemaLocation;
//	config.ValidationEventHandler += new ValidationEventHandler(ValidationCallBack);
//
//	// Get the XmlReader object with the configured settings.
//	XmlReader reader = XmlReader.Create(path, config);
//
//	// Parsing the file will cause the validation to occur.
//	while (reader.Read()) ;
//
//}

private void ValidationCallBack(object sender, ValidationEventArgs vea)
{
	if (vea.Severity == XmlSeverityType.Warning)
		Console.WriteLine(
			"\tWarning: Matching schema not found.  No validation occurred. {0}",
			vea.Message);
	else
		Console.WriteLine("\tValidation error: {0}", vea.Message);

}
public static IEnumerable<string> RecurseLocation(string basePath, string relpath,IEnumerable<string> patterns)
{
	
	
foreach(var pattern in patterns)
	foreach(var file in System.IO.Directory.GetFiles(System.IO.Path.Combine(basePath, relpath),pattern))
	{
		yield return file;
			
	}
	foreach(var dir in System.IO.Directory.GetDirectories(relpath))
	foreach(var result in  RecurseLocation(basePath,dir,patterns))
	yield return result;
}


public static class StringExtensions
{

public static string Delimit(this IEnumerable<string> values, string delimiter)
	{
	return values.Aggregate ((s1,s2)=>s1+delimiter+s2);
	}

public static string RemoveCharacters(this string text, IEnumerable<string> items)
	{
	var sb=new System.Text.StringBuilder(text);
	foreach(var item in items)
	{
	var oldVal=sb.ToString();
	sb.Clear();
	sb.Append(oldVal.Replace(item,string.Empty));
	}
	return sb.ToString();
	}
public static string RemoveMultipleWhitespaces(this string text)
	{
		return Regex.Replace(text,"\\s\\s+"," ");
	}
public static string TruncateTo(this string text, byte count)
	{
	if(text==null ||text.Length<=count)
	return text;
	return text.Substring(0,count);
	
	}
public static bool HasValue(this string text)
	{
	return string.IsNullOrEmpty(text)==false;
	}
	
	public static string[] SplitLines(this string text)
	{
		return text.Split(new string[] {"\r\n","\n"}, StringSplitOptions.None);
	}
	
	public static string StringAfter(this string text, string delimiter)
	{
		return text.Substring( text.IndexOf(delimiter)+delimiter.Length);
	}
	public static string StringAfterOrSelf(this string text, string delimiter)
	{
	if(text.Contains(delimiter)==false)
	return text;
	return text.StringAfter(delimiter);
	}
	
	
}