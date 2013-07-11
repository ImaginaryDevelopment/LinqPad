<Query Kind="Statements">
  <Reference>C:\Windows\System32\inetsrv\Microsoft.Web.Administration.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Runtime.dll</Reference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>Microsoft.Web.Administration</Namespace>
</Query>

var server= Util.ReadLine("Server?","vbcdapp1");
var iis=ServerManager.OpenRemote(server);

iis.Sites.SelectMany (ap =>
	ap.Applications.Select(a =>
			new{ 
				AppName=ap.Name+a.Path,
				AppPath=a.Path,
				Pool=a.ApplicationPoolName,
				Vd=a.VirtualDirectories.Select (vd =>
					new{vd.PhysicalPath,VirDir= vd.Path})}) ).Dump();



//
//var scopeFormatted=string.Format(@"\\{0}\root\MicrosoftIISv2", server);
//scopeFormatted.Dump();
//var scope = new ManagementScope(scopeFormatted);
//scope.Connect();
//var query= @"SELECT * FROM stdRegProv";
//
//var s= new SelectQuery(query);
//
//using(var mos=new ManagementObjectSearcher(scope,s))
//{
//	var items=mos.Get();
//	items.Dump();
//