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
var srcpath=Util.ReadLine("SourcePath?","$/Development");
var onlyLocks=false; //Util.ReadLine<bool>("only locked files?",true);
var minDate=DateTime.Today.AddMonths(-1);

	var tfsServer = Environment.GetEnvironmentVariable("servers").Dump().Split(new []{";"},StringSplitOptions.RemoveEmptyEntries).Dump().FirstOrDefault(c=>c.Contains("tfs"));
	var tfsUri= "https://"+tfsServer;
	
	Uri tfs08Uri= new Uri(tfsUri);
	
	var tfsUri=tfs08Uri;
	using(var tfsPc=new TfsTeamProjectCollection(tfsUri))
	
	{
		var vcs=tfsPc.GetService<Microsoft.TeamFoundation.VersionControl.Client.VersionControlServer>();
		
		var srcRoot=vcs.GetItem(srcpath);
		
		
		var pendings=vcs.QueryPendingSets(new[]{srcRoot.ServerItem}, RecursionType.Full,null,null).AsEnumerable();
		if(onlyLocks)
			pendings=pendings.Where(pq=>pq.PendingChanges.Any(pc=>pc.IsLock));
		var pendingQuery=pendings
			.Where(p=>p.PendingChanges.Any(pc=>pc.CreationDate>minDate)) 
			.OrderByDescending(p=>p.PendingChanges.Max(d=>d.CreationDate));
			
		var q= from pq in pendingQuery
				from pc in pq.PendingChanges
				//where pc.FileName.EndsWith(".refresh")==false
				select new{pq.OwnerName,pq.Computer,pc.ServerItem,pc.CreationDate,pc.FileName, pc.LockLevelName};
				var users=q.Select(u=>u.OwnerName).Distinct();
				var joinedQuery= from pc in q
					join l in lookup(users).ToList() on pc.OwnerName equals l.Key into luLeft
					from lu in luLeft.DefaultIfEmpty()
					let name=lu.Value??pc.OwnerName
					let serverItem=pc.ServerItem.Length>=srcpath.Length? pc.ServerItem.Remove(0,srcpath.Length):pc.ServerItem
					group  new{pc.CreationDate, pc.FileName,serverItem,pc.LockLevelName} by name into pcg
					select pcg;
					 joinedQuery.Dump();
				
				
	
		
	}
}
//snippets
//	var checkins=vcs.GetBranchHistory(new ItemSpec[]{new ItemSpec( gtpm.ServerItem, RecursionType.Full)},VersionSpec.Latest);
	//	checkins.Count().Dump();
	//var exclusionList=new [] {"*.suo","*.gpstate","svn","_svn",".svn","*Resharper*"};
		//	vcs.GetItems("$/Gstar/Monitoring/*.suo/", Microsoft.TeamFoundation.VersionControl.Client.RecursionType.Full).Items
		//		.Select(i=>i.ServerItem).Distinct().Dump();
		//var workspaces=vcs.QueryWorkspaces(null,null,null).Where(w=>w.IsLocal==false & w.LastAccessDate>DateTime.Now.AddDays(-15)).Where(w=>w.Folders.Any(f=> f.ServerItem.StartsWith("$/PST")));
		//workspaces.OrderByDescending(w=>w.LastAccessDate).Dump(1);

public static IEnumerable<KeyValuePair<string,string>> lookup(IEnumerable<string> nbIds)
{


using(var de=new System.DirectoryServices.DirectoryEntry())
{
	var customPath="LDAP://DC="+Environment.UserDomainName+",DC=local";
	de.Path=customPath;
	de.AuthenticationType= System.DirectoryServices.AuthenticationTypes.Secure;
	
	using(var deSearch=new System.DirectoryServices.DirectorySearcher())
	{
		deSearch.SearchRoot=de;
		var found=new Dictionary<string,string>( StringComparer.CurrentCultureIgnoreCase);
		foreach(var ownerName in nbIds)
			{
			if(found.ContainsKey(ownerName))
			yield return  found.First(k=>k.Key==ownerName);
			var closure=ownerName;
			if(closure.Contains("\\")) closure=closure.After("\\");
			//closure.Dump();
			deSearch.Filter="(&(objectClass=user)(SAMAccountName="+closure+"))";
			string value;
			try
			{	        
				var me=deSearch.FindOne();	
				value= me.Properties["name"][0].ToString();
				//value.Dump();
			}
			catch(Exception)
			{
				value= null;
			}
			 found.Add(ownerName,value);
			yield return new KeyValuePair<string,string>(ownerName,value);
		}
	}
}
}