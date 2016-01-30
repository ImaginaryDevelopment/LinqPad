<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.IO.Compression.FileSystem.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

//DeployDb
var sourceDbDir = @"C:\TFS\PracticeManagement\dev\PracticeManagement\ApplicationDatabase\sql\debug";
if (System.IO.Directory.Exists(sourceDbDir) == false)
{
	throw new DirectoryNotFoundException(sourceDbDir);
}
var targetFileName = Path.Combine(@"\\fs01\Documents\Brandon\PmDeploys","PmDb " + DateTime.Now.ToString("yyyyMMdd HHmm") +".zip");

System.IO.Compression.ZipFile.CreateFromDirectory(sourceDbDir,targetFileName);

targetFileName.AsDirPath().AsExplorerSelectLink("Zipped to").Dump();

