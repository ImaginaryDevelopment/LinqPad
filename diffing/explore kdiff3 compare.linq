<Query Kind="Program">
  <Connection>
    <ID>4e94eacc-a31d-4687-947b-e4c9804c895a</ID>
    <Persist>true</Persist>
    <Server>(local)</Server>
    <IncludeSystemObjects>true</IncludeSystemObjects>
    <Database>XPEncounter</Database>
    <ShowServer>true</ShowServer>
  </Connection>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>System.Threading.Tasks</Namespace>
</Query>

// explore branch differences
void Main()
{
	var local = Environment.ExpandEnvironmentVariables("%devroot%").Dump();
	if(Directory.Exists(local)==false){
		local.Dump("did not exist");
	}
	var trunk = @"C:\tfs\XpressCharts\Source";
	if(Directory.Exists(trunk)==false){
		trunk.Dump("did not exist");
	}
	
	string target = null;
//	var targetType = Util.ReadLine("Directory or file?","directory",new []{"directory","file"});
//	switch(targetType){
//		
//	}
	using(var dlg = new System.Windows.Forms.OpenFileDialog()){
		const string folderSelection = "Folder Selection.";
		dlg.InitialDirectory = local;
		dlg.CheckFileExists = false;
		dlg.CheckPathExists = true;
		dlg.ValidateNames=false;
		dlg.FileName=folderSelection;
		
		if(dlg.ShowDialog()!= System.Windows.Forms.DialogResult.OK)
		{
			"cancelled".Dump();
			return;
		}
		
		
		target= dlg.SafeFileName == folderSelection ? Path.GetDirectoryName(dlg.FileName) : dlg.FileName;
	}
	if(target.StartsWith(local)==false)
	{
		new{local,trunk,target}.Dump("this script will only compare a file in local to the mirror in trunk");
		return;
	}
	target.Dump("target acquired");
	
	var diff = Task.Run( ()=> 
		{
			var args = String.Join(" ",target.SurroundWith("\""),target.Replace(local,trunk).SurroundWith("\"")).Dump("args");
			return Util.Cmd(@"C:\Program Files\KDiff3\kdiff3.exe",args);
		});
	// explore tfs
	var baseProgFilesPath = @"Microsoft Visual Studio";
	
	var searchPath = Path.Combine(Environment.GetFolderPath( System.Environment.SpecialFolder.ProgramFilesX86));
	var tfsExes = 
		System.IO.Directory.EnumerateDirectories(searchPath, baseProgFilesPath+"*")
		.OrderByDescending(vsDir =>Decimal.Parse(vsDir.After(baseProgFilesPath)))
		.Dump("Vs directories")
		.SelectMany(d=> System.IO.Directory.EnumerateFiles(d,"tf.exe", SearchOption.AllDirectories))
		.Dump();
	var tfsExe = tfsExes.First();
	Util.Cmd(tfsExe,"vc status "+);
	GetHelp(tfsExe,TfsHelp.vc);
	diff.Result.Dump();
}

public enum TfsHelp{
	None,
	vc,
	git,
	settings
}
void GetHelp(string commandPath, TfsHelp helpArgument){
	string helpArg = null;
	switch(helpArgument){
		case TfsHelp.git:
		case TfsHelp.vc:
		case TfsHelp.settings:
			helpArg = helpArgument.ToString() +" help";
			break;
		default :
			helpArg = "?";
			break;
	}
	
	try{
		Util.Cmd(commandPath,helpArg);
	}
	catch (CommandExecutionException ex )
	{
		ex.Dump();	
	}
}