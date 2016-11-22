<Query Kind="Program">
  
</Query>

void Main()
{
	// explore sqlmetal
	var interestedItem = "UspPaymentsGet";
	var interestedType = "Function"; // Table or Function
	var targetPath = Path.Combine( Path.GetTempPath(),"linqpad_sqlmetal");
	Directory.CreateDirectory(targetPath);
	Environment.CurrentDirectory = targetPath;
	Environment.CurrentDirectory.Dump("cwd");
	var searchPath = Path.Combine(Environment.GetFolderPath( System.Environment.SpecialFolder.ProgramFilesX86), "Microsoft SDKs","Windows");
	var sqlMetals = System.IO.Directory.EnumerateFiles(searchPath,"sqlmetal.exe", SearchOption.AllDirectories).Dump();
	var sqlMetal = sqlMetals.First();
	
	var entireOutput = Util.Cmd(sqlMetal,"/conn:\""+ this.Connection.ConnectionString + "\" /sprocs",quiet:true);

	try
	{
		var doc = XDocument.Parse(string.Join(Environment.NewLine, entireOutput.SkipWhile(s => s.StartsWith("<") == false)));
		doc.DumpFormatted();
		switch (interestedType)
		{
			case "Function":
				var fun = doc.Root.Elements(doc.Root.Name.Namespace + "Function")
					//.Dump("functions")
					.FirstOrDefault(r => r.Attribute("Name").Value.EndsWith(interestedItem, StringComparison.InvariantCultureIgnoreCase)).Dump();
				if(fun != null)
					sys.sp_helptext(fun.Attribute("Name").Value).Dump();
			break;
		}
	}
	catch (Exception ex)
	{
		ex.Dump();
	}
	finally 
	{
		entireOutput.Where(l=> l.Contains("Warning") || l.Contains(interestedItem)).OrderByDescending(l => l.Contains(interestedItem)).Dump("interesting");
	}
	
	
	Directory.EnumerateFiles(Environment.CurrentDirectory).Dump("files in target dir");
	entireOutput.SkipWhile(s=>s.StartsWith("<")==false).SkipWhile(s=>s.Contains("<")).Dump("after doc output");
	GetCommandOptions(sqlMetal);
}

// Define other methods and classes here
void GetCommandOptions(string commandPath){
	
	try{
		Util.Cmd(commandPath,"/?");
	}
	catch (CommandExecutionException ex )
	{
		ex.Dump();	
	}
}