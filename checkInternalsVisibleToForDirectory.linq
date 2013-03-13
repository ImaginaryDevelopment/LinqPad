<Query Kind="Statements" />

var directory=@"C:\Program Files\Microsoft Visual Studio 10.0\Team Tools\Static Analysis Tools\FxCop\HackedMetrics";
string test;
var core=Assembly.ReflectionOnlyLoadFrom(@"C:\Windows\Microsoft.net\Framework\v4.0.30319\System.Core.dll");
core.GetReferencedAssemblies().Dump("Core references");

//System.Reflection.Assembly.GetExecutingAssembly().GetReferencedAssemblies().Dump();
//AppDomain.CurrentDomain.ReflectionOnlyGetAssemblies().Dump();
var q= from f in System.IO.Directory.GetFiles(directory,"*.dll")
	
		from a in System.Reflection.Assembly.ReflectionOnlyLoadFrom(f).GetCustomAttributesData()
	.Where(a=>a.Constructor.DeclaringType.Name.StartsWith("Internals"))
	.Select(ca2=>ca2.ConstructorArguments.Select(x=>x.Value.ToString()))
	.Aggregate((x1,x2)=>x1.Concat(x2))
		from f2 in System.IO.Directory.GetFiles(directory,"*.dll").Select(x=>System.IO.Path.GetFileNameWithoutExtension(x))
		where a.StartsWith(f2+",") || a==f2
			from refs in System.Reflection.Assembly.ReflectionOnlyLoadFrom(f).GetReferencedAssemblies()
			.Where(ra=>ra.GetPublicKeyToken()!=null)
		
		select new{File=System.IO.Path.GetFileName(f),a,refs.Name};


q.Distinct().Dump();