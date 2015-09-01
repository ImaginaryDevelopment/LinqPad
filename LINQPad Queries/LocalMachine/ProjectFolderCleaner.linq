<Query Kind="Statements" />

var target= Util.ReadLine("target?", @"c:\projects\");
var deletePackages=Util.ReadLine<bool>("delete packages?",false);
var searchDirs=new[]{ "_ReSharper*", "bin","obj"};
Func<string,bool> deleteDirectory= s=>{
	try
	{	        
		System.IO.Directory.Delete(s,true);
		return true;
	}
	catch (DirectoryNotFoundException ex)
	{
		
		ex.Message.Dump("failed to delete:"+s);
	}
	catch (IOException ioex){
		ioex.Message.Dump("failed to delete:"+s);
	}
	return false;
};
foreach(var searchPattern in searchDirs)
{
	var q=System.IO.Directory.GetDirectories(target,searchPattern, SearchOption.AllDirectories);
	var count=0;
	foreach(var d in q){
		if(deleteDirectory(d))
			count++;
	}
	count.Dump("deleted via "+searchPattern);
	
}
//package folders
var packageCount=0;
foreach(var p in System.IO.Directory.GetDirectories(target,"packages", SearchOption.AllDirectories)){
	foreach(var d in System.IO.Directory.GetDirectories(p)){
		
	//delete all subfolders, leaving packages.config and repositories.config alone
		if(deleteDirectory(d))
			packageCount++;
	}
}
packageCount.Dump("deleted via packages");

