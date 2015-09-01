<Query Kind="Program">
  <Namespace>Microsoft.Win32</Namespace>
</Query>

//customize windows run command
	//http://weblogs.asp.net/whaggard/archive/2004/04/11/111232.aspx
void Main()
{
string cmdPath=@"C:\Users\bdimperio\Desktop\cmds\";
if(System.IO.Directory.Exists(cmdPath)==false)
{
	Util.Highlight("need a valid cmd directory path").Dump("failed to validate pre-condition");
	return;
}
	ShowExistingAppPaths();
	AddAllInDirectory(cmdPath);
}
void AddAllInDirectory(string path)
{
	var appPaths=GetAppPathKey();
	var keys=appPaths.GetSubKeyNames();
	var q= from f in System.IO.Directory.GetFiles(path)
			let fOnly=System.IO.Path.GetFileNameWithoutExtension(f).ToLower()+".exe" //all keys must end in .exe even for .bat files
			where  f.EndsWith(".cmd") || f.EndsWith(".exe") //so far .bat and .lnk's to a .bat did not work
			join kl in keys.Where (k => k.EndsWith("exe", StringComparison.InvariantCultureIgnoreCase)).Select (k => k.ToLower())
				on fOnly equals kl into kLeft
			from k in kLeft.DefaultIfEmpty()
			
			
			select new{Name=fOnly,Path=f,ConflictingKey=k};
	q.Dump("filtered");
	q.Select (x =>new {x.Name, x.Path.Length}).Dump();
	var conflicts=q.Where (x => x.ConflictingKey!=null).Dump("Conflicted");
	var invalidShortcut=q.Where (x => x.Name.Contains(" ")).Dump("Bad shortcut");
	var toAdd=q
		.Where (x => x.ConflictingKey==null)
		.Select (x => new{ x.Name,x.Path}).ToArray();
	if(toAdd.Any ( ))
	{
		var writeKey=GetAppPathKey(writable:true);
		foreach(var app in toAdd)
		{
			var newAppPath=writeKey.CreateSubKey(app.Name);
			newAppPath.SetValue(string.Empty,app.Path);
		}
	}
	toAdd.Dump("added");
}

RegistryKey GetAppPathKey(bool writable=false)
{
var appPaths=Registry.LocalMachine.OpenSubKey(@"SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths", writable);
Debug.Assert(appPaths!=null);
return appPaths;
}
// Define other methods and classes here
void ShowExistingAppPaths()
{
var appPaths=GetAppPathKey();
	
	//appPaths.SubKeyCount.Dump("AppPaths");
	var q= from ap in appPaths.GetSubKeyNames()
			let sk=appPaths.OpenSubKey(ap)
			select new{ap,Value=sk.GetValue(string.Empty),Others=sk.GetValueNames().OrderBy (s => s).Where(s=>s.HasValue()).Select (s => new{Key= s,Value=sk.GetValue(s)})};
		q.Dump();
}