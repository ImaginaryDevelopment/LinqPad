<Query Kind="Program">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Common.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.DirectoryServices.dll</Reference>
  <Namespace>Microsoft.TeamFoundation.Client</Namespace>
  <Namespace>System.DirectoryServices</Namespace>
  <Namespace>Microsoft.TeamFoundation.VersionControl.Client</Namespace>
</Query>

void Main()
{
	//Uri tfs10Uri= new Uri("http://g-tfs.bankofamerica.com:8080/tfs/GSTAR");//GMS,HALOS,PSA,SAG
	Uri tfs08Uri= new Uri("https://tfs.oceansideten.com"); //GPD,Gstar/Monitoring,PST
	
	
	using(var tfsPc=new TfsTeamProjectCollection(tfs08Uri))
	
	{
		tfsPc.Dump();
		var vcs=tfsPc.GetService<Microsoft.TeamFoundation.VersionControl.Client.VersionControlServer>();
		
		var gtpm=vcs.GetItem("$/development/products/cvs/CVS.Member.Web4");
		
		
		var pendings=vcs.QueryPendingSets(new[]{gtpm.ServerItem}, RecursionType.Full,null,null);
		var pendingQuery=pendings.Where(p=>p.PendingChanges
				.Any(pc=>pc.CreationDate>DateTime.Today.AddMonths(-2))) 
			.OrderByDescending(p=>p.PendingChanges.Max(d=>d.CreationDate));
			
		var q= from pq in pendingQuery
				from pc in pq.PendingChanges
				where pc.FileName.EndsWith(".refresh")==false
				select new{pq.OwnerName,pq.Computer,pc.ServerItem,pc.CreationDate,pc.FileName};
				var users=q.Select(u=>u.OwnerName).Distinct().Dump();
				var joinedQuery= from pc in q
					join l in lookup(users).ToList() on pc.OwnerName equals l.Key into luLeft
					from lu in luLeft.DefaultIfEmpty()
					select new {Name=lu.Value??pc.OwnerName,pc.FileName,pc.ServerItem,pc.CreationDate,pc.Computer,pc.OwnerName};
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
	var customPath="LDAP://DC=RBIDev,DC=local";
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
			if(closure.StartsWith("RBIDEV\\"))
				closure=closure.Substring("RBIDEV\\".Length);
			closure.Dump();
			deSearch.Filter="(&(objectClass=user)(SAMAccountName="+closure+"))";
			string value;
			try
			{	        
				var user=deSearch.FindOne();	
				//user.Dump("user");
				value= user.Properties["name"][0].ToString();
				//value.Dump();
			}
			catch(Exception ex)
			{
			ex.Dump();
				value= null;
			}
			 found.Add(ownerName,value);
			yield return new KeyValuePair<string,string>(ownerName,value);
		}
	}
}
}