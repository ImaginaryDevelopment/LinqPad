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

//join TFS to Active Directory
void Main()
{
var srcpath=Util.ReadLine("SourcePath?","$/Development");
var onlyLocks=//false; 
	Util.ReadLine<bool>("only locked files?",true);
var minDate=DateTime.Today.AddDays(-30);

	var tfsServer = Environment.GetEnvironmentVariable("servers").Dump().Split(new []{";"},StringSplitOptions.RemoveEmptyEntries).Dump().FirstOrDefault(c=>c.Contains("tfs"));
	var tfsUri= "https://"+tfsServer;
	
	using(var tfsPc=new TfsTeamProjectCollection(new Uri(tfsUri)))
	
	{
		var vcs=tfsPc.GetService<Microsoft.TeamFoundation.VersionControl.Client.VersionControlServer>();
		
		var srcRoot=vcs.GetItem(srcpath);
		
		var pendings=vcs.QueryPendingSets(new[]{srcRoot.ServerItem}, RecursionType.Full,null,null).AsEnumerable();
		if(onlyLocks)
			pendings=pendings.Where(pq=>pq.PendingChanges.Any(pc=>pc.IsLock));
			
			//get users that have pending changes started before the cutoff and still haven't checked in
		var pendingQuery=pendings.Where(p=>p.PendingChanges
			
				.Any(pc=>pc.CreationDate<minDate && pc.Undone==false) )  
			.OrderByDescending(p=>p.PendingChanges.Max(d=>d.CreationDate));
			
		var q= from pq in pendingQuery
				
				select pq.OwnerName;
			var users=q.Distinct();	
			var qw= from u in users
					join l in lookup(users).ToList() on u equals l.Key into luLeft
					from lu in luLeft.DefaultIfEmpty()
					let w=vcs.QueryWorkspaces(null,u,null)
					select new{u,lu,Workspaces=w.Select(ws=>new{ws.Name,ws.Computer,ws.OwnerName})};
					
			qw.Select(a=>new{user=a.lu.Value, userID=a.u,Workspaces=a.Workspaces.Select(b=>new{b.Name,b.Computer})}).Dump("workspaces");
			var baseUndo="tf undo \"{0}\" /WORKSPACE:{1};{2} /recursive /server:{3}";

						
							
							
							
			foreach(var u in qw.OrderBy(o=>o.lu.Value.IsNullOrEmpty()?"zz":
				o.lu.Key.Contains("\\NBD") || o.lu.Key.Contains("\\NBS")?"z"+o.lu.Value:o.lu.Key))
			{
			
			var sb=new StringBuilder();
				foreach(var w in u.Workspaces)
				{
					sb.AppendLine(String.Format(baseUndo,srcpath,w.Name,u.u,tfsUri.ToString()));
					
				}
				sb.ToString().Dump("undo command(s) for "+ u.lu.Value);
			}

		
	}
}

public static IEnumerable<KeyValuePair<string,string>> lookup(IEnumerable<string> nbIds)
{


using(var de=new System.DirectoryServices.DirectoryEntry())
{
//CN=Brandon D'Imperio,OU=Development,OU=Oceanside Ten,DC=RBIDev,DC=local
	var customPath="LDAP://DC="+Environment.UserDomainName+",DC=local";
	de.Path=customPath;
	de.AuthenticationType= System.DirectoryServices.AuthenticationTypes.Secure;
	
	using(var deSearch=new System.DirectoryServices.DirectorySearcher())
	{
		deSearch.SearchRoot=de;
		var found = new Dictionary<string,string>( StringComparer.CurrentCultureIgnoreCase);
		foreach(var ownerName in nbIds)
			{
			if(found.Dump("found").ContainsKey(ownerName.Dump("ownername")))
			yield return  found.First(k=>k.Key==ownerName);
			var closure=ownerName;
			if(closure.Contains("\\")) closure=closure.After("\\");
			//closure.Dump();
			deSearch.Filter="(&(objectClass=user)(SAMAccountname="+closure+"))";
			string value;
			
			try
			{	        
				var me=deSearch.FindOne();	
				value= me.Properties["name"][0].ToString();
				// me.Properties.Dump("properties of "+ownerName);
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