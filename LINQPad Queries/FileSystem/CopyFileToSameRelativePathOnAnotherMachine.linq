<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

//System.Reflection.Assembly.ReflectionOnlyLoad("System.Threading.Tasks.Dataflow")
var copyNeeded =
	new[] {
	@"C:\Program Files (x86)\Common Files\microsoft shared\VsHub\1.0.0.0\lib\System.Threading.Tasks.Dataflow.dll" // needs to be gac'd too
	};
var clip = System.Windows.Forms.Clipboard.GetFileDropList().Dump("from clipboard");
if (clip == null || clip.Count < 1)
{
	"no files found in clipboard,aborting".Dump();
	return;
}
var appDomain = AppDomain.CurrentDomain;
appDomain.AssemblyResolve += (sender, obj) =>
 {
 	return null;
 };
Func<string, Assembly> tryReflectionLoad = fullPath =>
	 {
		 try
		 {
			 return Assembly.ReflectionOnlyLoad(Path.GetFileName(fullPath));
		 }
		 catch (Exception ex1)
		 {
			 ex1.Dump("tryReflectionLoadFailed");
			 try
			 {
				 return Assembly.ReflectionOnlyLoad(Path.GetFileNameWithoutExtension(fullPath));
			 }
			 catch (Exception ex2)
			 {
				 ex2.Dump("tryReflectionAlternateLoadFailed");
				 fullPath.Dump("trying to load from the copy source");
				 try
				 {
					 var directLoad = Assembly.ReflectionOnlyLoad(fullPath);
					 var fullName = directLoad.FullName;
					 try
					 {
						 return Assembly.ReflectionOnlyLoad(fullName).Dump("could be loading the one we already loaded =(");
					 }
					 catch (Exception ex4)
					 {

						 ex4.Dump("loaded from targetFile, but still could not load via fullname");
					 }
				 }
				 catch (Exception ex3)
				 {
					 ex3.Dump("failed to load from the target, could be processor mismatch or ?");
					 throw;
				 }

				 return null;
			 }
		 }
	 };
var targetMachine = "build2010";

Action<string, string> copyFileRelative = (srcPath, targetPath) =>
{
	if (!Directory.Exists(Path.GetDirectoryName(targetPath)))
	{
		var newDir = Path.GetDirectoryName(targetPath);
		Directory.CreateDirectory(newDir);
		newDir.Dump("created");
	}
	File.Copy(srcPath, targetPath);
	targetPath.Dump("copied");
};

foreach (var filePath in clip)
{
	var path = Path.IsPathRooted(filePath) ? filePath : Path.GetFullPath(filePath);
	var isGac = path.EndsWith(".dll") && tryReflectionLoad(path).Dump("reflectionLoaded") != null;
	isGac.Dump("isGac");
	if (File.Exists(path))
	{
		path.Dump("already exists, skipping");
		continue;
	}

	var targetPath = $"\\\\{targetMachine}\\c$\\{path.After(":\\")}";

	if (Directory.Exists(path)) // targetPath will be a directory
	{
		new { Src = path, TargetPath = targetPath }.Dump("directory copy needed");

		//breadth first or depth first?
		//		foreach (var file in Directory.EnumerateFiles(path))
		//		{
		//			File.Copy(file,targetPath);
		//		}
	}
	else
	{
		copyFileRelative(path, targetPath);
	}



}
//&& File.Exists(clip)