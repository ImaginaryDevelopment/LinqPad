<Query Kind="Program">
  <Namespace>System.Threading.Tasks</Namespace>
</Query>

void Main()
{
	var targets=new []{@"C:\Windows\Microsoft.NET\Framework\v2.0.50727\Temporary ASP.NET Files\*.*",
	@"C:\Windows\Microsoft.NET\Framework\v4.0.30319\Temporary ASP.NET Files\*.*",
	@"C:\Windows\Microsoft.NET\Framework64\v2.0.50727\Temporary ASP.NET Files\*.*",
	@"C:\Windows\Microsoft.NET\Framework64\v4.0.30319\Temporary ASP.NET Files\*.*",
	};
	var result=Parallel.ForEach(targets,i=>{if(System.IO.Directory.Exists(i)) System.IO.Directory.Delete(i,true);});
	result.Dump();
	using (var ps=new Process())
	{
	ps.StartInfo.FileName="iisreset";
	var output=ps.RunProcessRedirected("/start");
	output.Dump();
	}
}

// Define other methods and classes here
