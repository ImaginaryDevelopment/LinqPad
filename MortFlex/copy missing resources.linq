<Query Kind="Program" />

void Main()
{
	var subPaths = new[]{
		"LoanQuest Origination",
		"LoanQuest Transport Server"
	};
	var targetPath= @"C:\Microsoft .Net 3.5 Framework\Mortgageflex products\";
	var sourcePath= @"C:\Microsoft .Net 3.5 Framework\MORTGAGEFLEX PRODUCTS_77461\";
	var additionalFiles = new[]{
		@"Application\LoanQuest\AppStyles\Office2007Blue.isl",
		//@"Application\Mortgageflex.LoanQuest.ImageDrop\AppStyles\Office2007Blue.isl",
	@"Application\LoanQuest\Cache\Temp.txt", @"Application\LoanQuest\LocalStore\MyUserDictionary.txt", @"Application\LoanQuest\TempCompile\Temp.txt"};
	foreach(var af in additionalFiles){
		var targetFile=System.IO.Path.Combine( targetPath, af);
		var sourceFile= System.IO.Path.Combine(sourcePath,af);
		if(System.IO.File.Exists(targetFile)==false && System.IO.File.Exists(sourceFile)){
			System.IO.File.Copy(sourceFile.Dump("copy to "+targetFile),targetFile);
		}
	}
	
	foreach(var sp in subPaths)
		CopyResources(System.IO.Path.Combine(sourcePath,sp),System.IO.Path.Combine(targetPath,sp));
}
void CopyResources(string sourcePath,string targetPath){
foreach(var resourceFolder in System.IO.Directory.GetDirectories(targetPath,"resources", SearchOption.AllDirectories))
	{
		if(System.IO.Directory.GetFiles(resourceFolder).Any()==false)
		{
		//resourceFolder.Dump();
		var relativePath = resourceFolder.Substring(targetPath.Length+1).Dump();
		var sourceFullPath= System.IO.Path.Combine(sourcePath, relativePath).Dump("source full path");
		foreach(var sf in System.IO.Directory.EnumerateFiles(sourceFullPath).Where(a=>a.EndsWith(".scc")==false)){
			var targetFile= System.IO.Path.Combine(targetPath,relativePath,System.IO.Path.GetFileName(sf));
			sf.Dump("copy to "+targetFile);
			System.IO.File.Copy(sf,targetFile);
		}
		}
		
	}
}
// Define other methods and classes here
