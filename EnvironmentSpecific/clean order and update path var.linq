<Query Kind="Statements" />

Environment.GetEnvironmentVariable("path").Dump();
var oldPath=Environment.GetEnvironmentVariable("path", EnvironmentVariableTarget.Machine).Dump();
var paths = oldPath.Split(new []{";"}, StringSplitOptions.RemoveEmptyEntries).Select(p => new{ Path=p,Exists = Directory.Exists(p)}).Dump();
//var newPath= oldPath.Before("casper").Dump()+"casper\\bin";
var toAdd=Util.ReadLine("path");
if(System.IO.Directory.Exists(toAdd)==false)
	return;
var pathVars= oldPath.Split(';').Prepend(toAdd).ToList().Select(p=>new{p,Exists= System.IO.Directory.Exists(p)}).Dump();
var newPath=pathVars
	.Where (v => v.Exists)
	.Select (v => v.p)
	.OrderBy (v => !v.StartsWith("C:\\WINDOWS", StringComparison.InvariantCultureIgnoreCase))
	.ThenBy (v => v.Contains("x86"))
	.ThenBy (v =>v.Contains("SDK")|| v.Contains("Windows Kits")|| v.Contains("ATI")||v.Contains("AMD"))
	.Delimit(";")
	.Dump("newPath");
	
Environment.SetEnvironmentVariable("path",newPath, EnvironmentVariableTarget.Machine);