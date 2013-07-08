<Query Kind="Statements">
  <Connection>
    <ID>c9b76ea2-c7e7-4a9e-a336-ec4241c6dac5</ID>
    <Persist>true</Persist>
    <Server>rpsql2008r2dev</Server>
    <SqlSecurity>true</SqlSecurity>
    <Database>NOVA08RLS</Database>
    <UserName>WINRLS</UserName>
    <Password>AQAAANCMnd8BFdERjHoAwE/Cl+sBAAAA1WLKs9qc4USFiwcJ5tmkhgAAAAACAAAAAAADZgAAwAAAABAAAACrzDqjgelbVgZelHzxoUCkAAAAAASAAACgAAAAEAAAANaxhqC+PlSpfWS3MUIfULoIAAAAIqZ2P2kltAwUAAAAsZVmuOxGnoVr0wOGD/We2Jqm8Z4=</Password>
  </Connection>
</Query>

var targetPath= @"C:\Microsoft .Net 3.5 Framework\Mortgageflex products\LoanQuest Origination";
var sourcePath= @"C:\Microsoft .Net 3.5 Framework\MORTGAGEFLEX PRODUCTS_77461\LoanQuest Origination";
var additionalFiles = new[]{@"Application\LoanQuest\AppStyles\Office2007Blue.isl",@"Application\Mortgageflex.LoanQuest.ImageDrop\AppStyles\Office2007Blue.isl",
@"Application\LoanQuest\Cache\Temp.txt", @"Application\LoanQuest\LocalStore\MyUserDictionary.txt", @"Application\LoanQuest\TempCompile\Temp.txt"};
foreach(var af in additionalFiles){
	var targetFile=System.IO.Path.Combine( targetPath, af);
	if(System.IO.File.Exists(targetFile)==false){
		System.IO.File.Copy(System.IO.Path.Combine(sourcePath,af).Dump("copy to "+targetFile),targetFile);
	}
}


foreach(var resourceFolder in System.IO.Directory.GetDirectories(targetPath,"resources", SearchOption.AllDirectories))
{
	if(System.IO.Directory.GetFiles(resourceFolder).Any()==false)
	{
	//resourceFolder.Dump();
	var relativePath = resourceFolder.Substring(targetPath.Length+1).Dump();
	var sourceFullPath= System.IO.Path.Combine(sourcePath, relativePath).Dump("source full path");
	foreach(var sf in System.IO.Directory.EnumerateFiles(sourceFullPath)){
		var targetFile= System.IO.Path.Combine(targetPath,relativePath,System.IO.Path.GetFileName(sf));
		sf.Dump("copy to "+targetFile);
		System.IO.File.Copy(sf,targetFile);
	}
	}
	
}
