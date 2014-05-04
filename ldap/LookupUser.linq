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
	lookup(new[]{"Brandon D'Imperio","brandon.dimperio"}).Dump();
}

// Define other methods and classes here
public static IEnumerable<KeyValuePair<string,string>> lookup(IEnumerable<string> nbIds)
	{
	
	
	using(var de=new System.DirectoryServices.DirectoryEntry())
	{
	//CN=Brandon D'Imperio,OU=Development,OU=Oceanside Ten,DC=RBIDev,DC=local
		var customPath="LDAP://DC=RBIDev,DC=local";
		de.Path=customPath;
		de.AuthenticationType= System.DirectoryServices.AuthenticationTypes.Secure;
		
		using(var deSearch=new System.DirectoryServices.DirectorySearcher())
		{
			deSearch.SearchRoot=de;
			var found=new Dictionary<string,string>( StringComparer.CurrentCultureIgnoreCase);
			foreach(var ownerName in nbIds)
				{
				if(found.Dump("found").ContainsKey(ownerName.Dump("ownername")))
					yield return  found.First(k=>k.Key==ownerName);
				var closure=ownerName;
				//if(closure.StartsWith("CORP\\")) closure=closure.Substring("CORP\\".Length);
				//closure.Dump();
				deSearch.Filter="(&(objectClass=user)(sAMAccountName="+closure+"))";
				string value;
				try
				{	        
					var me=deSearch.FindOne();	
					me.Dump();
					value= me.Properties["name"][0].ToString();
					
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