<Query Kind="Program">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Build.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Build.Common.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Common.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.dll</Reference>
  <Namespace>Microsoft.TeamFoundation.Client</Namespace>
  <Namespace>Microsoft.TeamFoundation.VersionControl.Client</Namespace>
  <Namespace>System.DirectoryServices</Namespace>
</Query>

void Main()
{
var srcpath=Util.ReadLine("SourcePath?","$/");
var onlyLocks=false; //Util.ReadLine<bool>("only locked files?",true);
DateTime? minDate=DateTime.Today.AddMonths(-12);

	var tfsServer = Environment.GetEnvironmentVariable("servers")
		.Dump()
		.Split(new []{";"},StringSplitOptions.RemoveEmptyEntries)
		.Dump()
		.FirstOrDefault(c=>c.Contains("tfs"));
		
	var tfsUriText= "https://"+tfsServer;
	
	Uri tfsUri= new Uri(tfsUriText);
	
	using(var tfsPc=new TfsTeamProjectCollection(tfsUri))
	
	{
		var vcs=tfsPc.GetService<Microsoft.TeamFoundation.VersionControl.Client.VersionControlServer>();
		
		var srcRoot=vcs.GetItem(srcpath);
		
		
		var pendings=vcs.QueryPendingSets(new[]{srcRoot.ServerItem}, RecursionType.Full,null,null).AsEnumerable();
		if(onlyLocks)
			pendings=pendings.Where(pq=>pq.PendingChanges.Any(pc=>pc.IsLock));
		if(minDate.HasValue)
			pendings=pendings.Where(pq => pq.PendingChanges.Any( pc => pc.CreationDate > minDate.Value));
		var pendingQuery=pendings
			.OrderByDescending(p=>p.PendingChanges.Max(d=>d.CreationDate));
		pendingQuery.Dump("pending");	
		
		
	}
}

