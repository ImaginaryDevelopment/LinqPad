<Query Kind="Statements" />

// search gac
// http://stackoverflow.com/a/1599740/57883
// List of all the different types of GAC folders for both 32bit and 64bit
// environments.
List<string> gacFolders = new List<string>() { 
    "GAC", "GAC_32", "GAC_64", "GAC_MSIL", 
    "NativeImages_v2.0.50727_32", 
    "NativeImages_v2.0.50727_64",
    "NativeImages_v4.0.30319_32",
    "NativeImages_v4.0.30319_64"
};

var q = from folder in gacFolders
		let gacFullPath = Path.Combine(Environment.ExpandEnvironmentVariables(@"%systemroot%\assembly"),folder)
		where Directory.Exists(gacFullPath)
		from asmFullPath in Directory.GetDirectories(gacFullPath)
		from vFullPath in Directory.GetDirectories(asmFullPath)
		//where asmFullPath.Contains("Telerik",StringComparison.InvariantCultureIgnoreCase)
		from file in Directory.GetFiles(vFullPath)
		let versionInfo = FileVersionInfo.GetVersionInfo(file)
		let fileVersion = versionInfo.FileVersion
		let productVersion = versionInfo.ProductVersion
		select new{Folder= Path.GetDirectoryName(gacFullPath),Gac=Path.GetFileName(gacFullPath),Asm=Path.GetFileName(asmFullPath),Ver=Path.GetFileName(vFullPath),File=Path.GetFileName(file),fileVersion,productVersion };
		
		q.Dump();
