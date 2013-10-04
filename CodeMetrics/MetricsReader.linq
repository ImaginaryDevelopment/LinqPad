<Query Kind="Program">
  <Namespace>System.Globalization</Namespace>
</Query>

void Main()
{
	var sourceDirectory=System.IO.Path.Combine(
		System.Environment.GetFolderPath( System.Environment.SpecialFolder.Desktop),"measures");
	var measures=GetMembers(sourceDirectory).AsParallel().ToArray();
	measures.Where (m => m.Mi.HasValue && m.Mi<20)
	.ToArray()
	.Where (m => m.CompanyName.Value!="Data Dynamics")
	.Take(100)
	.OrderBy (m => m.Mi).Dump("Low MI");
	measures.Where (m =>m.LoC>300  ).Take(100).OrderBy (m => m.LoC).Dump("High Loc");
}
internal static IEnumerable<Measurement> GetMembers(string path)
{

	foreach(var item in System.IO.Directory.GetFiles(path,"*.xml"))
	{
		var info=new FileInfo(item);
		if(info.Length<1)
		{
			info.Delete();
			continue;
		}
		var x=XElement.Load(item);
		foreach(var trg in x.XPathSelectElements("//Target"))
		foreach(var mod in trg.XPathSelectElements("//Module"))
		{
			var targetName=trg.GetNameAttr();
			var targetInfo=new Lazy<FileInfo>(()=>new FileInfo(targetName));
			var company=new Lazy<string>(()=>System.Reflection.Assembly.ReflectionOnlyLoadFrom(targetName).GetAssemblyReflectionAttribute<AssemblyCompanyAttribute>());
			
			var modMetrics=mod.GetNodeMetrics();
			
			foreach(var ns in mod.XPathSelectElements("//Namespace"))
			{
				var nsMetrics=ns.GetNodeMetrics();
				foreach(var t in ns.XPathSelectElements("//Type"))
				{
					var tMetrics=t.GetNodeMetrics();
					foreach(var member in t.XPathSelectElements("//Member"))
					{
						var metrics= member.GetNodeMetrics();	
						
						var fullInfo=new Measurement(){ 
							CompanyName=company //.Value,
							,
							Outfile=info.Name,
							Target=targetName,
							Module=mod.GetNameAttr(),
							Namespace=ns.GetNameAttr(),
							Type=t.GetNameAttr(),
							Member=member.GetNameAttr(),
							Cc=metrics.Cc, Mi=metrics.Mi, LoC=metrics.LoC  };
						yield return fullInfo;
						//new Measurement(){ Target=t.Attribute(XNamespace.None+ "Name").Value, Module=mod.Attribute(XNamespace.None+"Name").Value, Namespace=ns.
					}
				}
			}
		}
		
	
	}
	yield break;
}
public static class Extensions{
	internal static string GetAssemblyReflectionAttribute<T>(this Assembly a)
	where T:Attribute
	{
		var cads=a.GetCustomAttributesData();
		var result=cads
				.FirstOrDefault (aca => aca.Constructor.DeclaringType==typeof(T))
				.ConstructorArguments.Select (ca => ca.Value.ToString())
				.FirstOrDefault() ;
		return result;
	}
	internal static int? TryParse(this string s,NumberStyles? ns=null)
	{
		int tmp=0;
		int? result=null;
		if(ns.HasValue)
		{
			if(int.TryParse(s,ns.Value,CultureInfo.InvariantCulture, out tmp))
				result=tmp;
				
				return result;
		}
		if(int.TryParse(s,out tmp))
			result=tmp;
		return result;
	}
	internal static string GetNameAttr(this XElement x)
	{
		return x.Attribute(XNamespace.None+"Name").Value;
	}
	internal static MetricSet GetNodeMetrics(this XElement x)
	{
		var m=x.XPathSelectElement("Metrics");
		
		if(m==null)
			x.Dump("found null");	
		
		var mi=m.XPathSelectElement("Metric[@Name='MaintainabilityIndex']").Attribute("Value").Value.TryParse(NumberStyles.AllowThousands);
			
		var cc=m.XPathSelectElement("Metric[@Name='CyclomaticComplexity']").Attribute("Value").Value.TryParse(NumberStyles.AllowThousands);
		
		var loc=m.XPathSelectElement("Metric[@Name='LinesOfCode']").Attribute("Value").Value.TryParse(NumberStyles.AllowThousands);
		return new MetricSet(){ Cc=cc, Mi=mi, LoC=loc};
		
	}
}
class MetricSet{
	public int? Cc { get; set; }
	public int? Mi { get; set; }
	public int? LoC{get;set;}
}
// Define other methods and classes here
class Measurement:MetricSet{
	public Lazy<string> CompanyName{get;set;}
	public string Outfile{get;set;}
	public string Target { get; set; }
	public string Module{get;set;}
	public string Namespace { get; set; }
	public string Type { get; set; }
	public string Member{get;set;}
}