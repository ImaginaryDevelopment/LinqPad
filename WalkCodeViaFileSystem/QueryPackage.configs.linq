<Query Kind="Statements" />

//bool debug=false;
var baseDir=Util.ReadLine("Directory?",System.Environment.GetEnvironmentVariable("devroot"));
var packageConfigs= System.IO.Directory.GetFiles(baseDir,"packages.config", SearchOption.AllDirectories);

var references = from r in packageConfigs
where r.Contains("Playground")==false
			from pc in System.IO.File.ReadAllLines(r)
			where pc.Contains("id=\"")
			orderby pc
			select new{r,pc};
			references.Dump();
packageConfigs.Dump();