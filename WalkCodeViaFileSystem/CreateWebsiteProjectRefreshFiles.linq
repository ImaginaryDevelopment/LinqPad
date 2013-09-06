<Query Kind="Statements" />

//create .refresh file for .dlls

var targetPaths = new []{
@"C:\Microsoft .Net 3.5 Framework\Mortgageflex products\Common Framework\Host\Mortgageflex.Services.Host.Registration\",
@"C:\Microsoft .Net 3.5 Framework\Mortgageflex products\Common Framework\Host\Mortgageflex.Services.Host.DataLookupService\",
@"C:\Microsoft .Net 3.5 Framework\Mortgageflex products\Common Framework\Host\Mortgageflex.Services.Host.LoanQuest\",
@"C:\Microsoft .Net 3.5 Framework\Mortgageflex products\Common Framework\Host\Mortgageflex.Services.Host.ProgramPricing\",
@"C:\Microsoft .Net 3.5 Framework\Mortgageflex products\Common Framework\Host\Mortgageflex.Services.Host.PrintingService\",
@"C:\Microsoft .Net 3.5 Framework\Mortgageflex products\Common Framework\Host\Mortgageflex.Services.Host.GatewayService\"

};
Debug.Assert(targetPaths.All(a=>a.EndsWith(@"\")),"Directories must end with closing \\");
var fileTypes = new []{ "*.dll", "*.pdb"};
var referencePath = @"C:\Microsoft .Net 3.5 Framework\references\";
foreach(var websiteProjectBinPath in targetPaths.Select(a=>a+"bin"))
{

	var relative =websiteProjectBinPath.AsDirPath().GetRelativePathTo(referencePath).Dump();//  new Uri(referencePath).MakeRelativeUri(new Uri(targetPath)).ToString().Dump();
	var existingRefreshes =System.IO.Directory.EnumerateFiles(websiteProjectBinPath,"*.refresh").Select(a=>System.IO.Path.GetFileNameWithoutExtension(a)).ToArray().Dump("already using refresh");

	foreach(var fileType in fileTypes)
	{

		var canRefresh = from dll in System.IO.Directory.EnumerateFiles(websiteProjectBinPath,fileType).Select(System.IO.Path.GetFileName)
			where existingRefreshes.Contains(dll)==false
			join dRef in System.IO.Directory.EnumerateFiles(referencePath,fileType).Select(System.IO.Path.GetFileName) on dll equals dRef
			select dll;
	
		foreach(var toCreate in canRefresh){
			var targetFilePath = System.IO.Path.Combine(websiteProjectBinPath,toCreate+".refresh").Dump();
			//new{ targetFilePath, relative,toCreate}.Dump();
			System.IO.File.WriteAllText(targetFilePath,relative.Replace("/",@"\")+toCreate);
		}
		var couldNotReferesh = from dll in System.IO.Directory.EnumerateFiles(websiteProjectBinPath,fileType).Select(System.IO.Path.GetFileName)
			where existingRefreshes.Contains(dll)==false
			where canRefresh.Contains(dll)==false
			select dll;
	
		couldNotReferesh.Dump("could not refresh");
	}
}