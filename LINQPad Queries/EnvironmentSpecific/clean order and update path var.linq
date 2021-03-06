<Query Kind="Statements" />

Environment.GetEnvironmentVariable("path").Dump();
var oldPath=Environment.GetEnvironmentVariable("path", EnvironmentVariableTarget.Machine).Dump();
var paths = oldPath.Split(new []{";"}, StringSplitOptions.RemoveEmptyEntries).Select(p => new{ Path=p,Exists = Directory.Exists(p)}).Dump();
//var newPath= oldPath.Before("casper").Dump()+"casper\\bin";
Func<string[], string, string[]> prepend = (items, newHead) => new[] { newHead}.Concat(items).ToArray();
var pathVars= oldPath.Split(';').ToList().Select(p=>new{p,Exists= System.IO.Directory.Exists(p)}).Dump();
if(Util.ReadLine<bool>("Clean?"))
{

	var newPath=pathVars
		.Where (v => v.Exists)
		.Select (v => v.p)
		.OrderBy (v => !v.StartsWith("C:\\WINDOWS", StringComparison.InvariantCultureIgnoreCase))
		.ThenBy (v => v.Contains("x86"))
		.ThenBy (v =>v.Contains("SDK")|| v.Contains("Windows Kits")|| v.Contains("ATI")||v.Contains("AMD"))
		.Aggregate((s1,s2) => s1 + ";" + s2)
		.Dump("newPath");
	// may require this process runs as admin
	Environment.SetEnvironmentVariable("path",newPath, EnvironmentVariableTarget.Machine);
}

var toAdd=Util.ReadLine("path to add?(empty for no additions)");
if(string.IsNullOrEmpty(toAdd) || System.IO.Directory.Exists(toAdd)==false)
	return;
pathVars= prepend(oldPath.Split(';'),toAdd).ToList().Select(p=>new{p,Exists= System.IO.Directory.Exists(p)}).Dump();
var newPath2=pathVars
	.Where (v => v.Exists)
	.Select (v => v.p)
	.OrderBy (v => !v.StartsWith("C:\\WINDOWS", StringComparison.InvariantCultureIgnoreCase))
	.ThenBy (v => v.Contains("x86"))
	.ThenBy (v =>v.Contains("SDK")|| v.Contains("Windows Kits")|| v.Contains("ATI")||v.Contains("AMD"))
	.Aggregate((s1,s2) => s1 + ";" + s2)
	.Dump("newPath");
if(Util.ReadLine<bool>("do Sort?"))
    Environment.SetEnvironmentVariable("path",newPath2, EnvironmentVariableTarget.Machine);