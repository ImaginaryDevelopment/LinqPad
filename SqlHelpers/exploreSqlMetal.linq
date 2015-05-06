<Query Kind="Program">
  <Connection>
    <ID>4e94eacc-a31d-4687-947b-e4c9804c895a</ID>
    <Persist>true</Persist>
    <Server>(local)</Server>
    <IncludeSystemObjects>true</IncludeSystemObjects>
    <Database>XPApplication</Database>
    <ShowServer>true</ShowServer>
  </Connection>
</Query>

void Main()
{
	// explore sqlmetal
	var interestedItem = "RegistrationCode";
	
	var targetPath = Path.Combine( Path.GetTempPath(),"linqpad_sqlmetal");
	Directory.CreateDirectory(targetPath);
	Environment.CurrentDirectory = targetPath;
	Environment.CurrentDirectory.Dump("cwd");
	var searchPath = Path.Combine(Environment.GetFolderPath( System.Environment.SpecialFolder.ProgramFilesX86), "Microsoft SDKs","Windows");
	var sqlMetals = System.IO.Directory.EnumerateFiles(searchPath,"sqlmetal.exe", SearchOption.AllDirectories).Dump();
	var sqlMetal = sqlMetals.First();
	
	var entireOutput = Util.Cmd(sqlMetal,"/conn:\""+ this.Connection.ConnectionString + "\" /sprocs",quiet:true);
	entireOutput.Where(l=> l.Contains("Warning") || l.Contains(interestedItem)).Dump("interesting");
	
	var doc = XDocument.Parse(string.Join(Environment.NewLine, entireOutput.SkipWhile(s=>s.StartsWith("<")==false)));
	doc.DumpFormatted();
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