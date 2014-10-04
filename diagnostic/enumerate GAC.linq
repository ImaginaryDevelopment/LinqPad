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

foreach( string folder in gacFolders)
{
	string path = Path.Combine(Environment.ExpandEnvironmentVariables(@"%systemroot%\assembly"),folder);
	if(Directory.Exists(path))
	{
	
		Directory.GetDirectories(path).Dump();
		//foreach( var assemblyFolder in Directory.GetDirectories(path)){
			
		//}
	}
}