<Query Kind="Program">
  <Namespace>System.Threading.Tasks</Namespace>
</Query>

void Main()
{
	//Environment.GetEnvironmentVariables().Dump();
	foreach(var srv in Environment.GetEnvironmentVariable("servers").Dump().Split(';').Except(new []{"ts2.oceansideten.com","tfs.oceansideten.com"}).AsParallel())
	{
		var stopWatch = new Stopwatch();
		stopWatch.Start();
		var buildsPath=@"\\" + srv +@"\c$\Builds";
		if(!System.IO.Directory.Exists(buildsPath))
			continue;
		srv.Dump("has builds folder!");
		var q = from agentFolder in System.IO.Directory.EnumerateDirectories(buildsPath)
				from teamFolder in System.IO.Directory.EnumerateDirectories(agentFolder)
				from buildFolder in System.IO.Directory.EnumerateDirectories(teamFolder) 
				let buildFolderPath = new My.PathWrapper(buildFolder)
				let srcFolder = buildFolderPath.Combine("src").Dump("src!")
				let logs = System.IO.Directory.EnumerateFiles(srcFolder.RawPath,"*.log", SearchOption.AllDirectories).ToArray()
				select new{srv,agentFolder,teamFolder,
					BuildFolder=buildFolderPath.AsExplorerSelectLink(buildFolder),
					LogCount = logs.Length,
					Logs=logs.Select(l=>new{Log=new My.PathWrapper(l).AsExplorerSelectLink(System.IO.Path.GetFileNameWithoutExtension(l))})
				
				};
		q.Dump();
		stopWatch.Stop();
		stopWatch.Dump(srv);
	
		
	}
}

// Define other methods and classes here
