<Query Kind="Statements" />

// search product and file version info
List<string> interestedFolders = new List<string>() { 
    @"C:\TFS\Pm-Rewrite\Source-dev-rewrite\PracticeManagement\3rdParty",
	@"C:\TFS\Pm-Rewrite\Source-dev-rewrite\PracticeManagement\bin",
	@"C:\Program Files (x86)\Telerik\"
};

var q = from folder in interestedFolders
		from fileFullPath in Directory.GetFiles(folder,"*.dll", SearchOption.AllDirectories)
		where fileFullPath.Contains("telerik.windows", StringComparison.InvariantCultureIgnoreCase)
		let file = Path.GetFileName(fileFullPath)
		where file.Contains("telerik", StringComparison.InvariantCultureIgnoreCase)
		let versionInfo = FileVersionInfo.GetVersionInfo(fileFullPath)
		let fileVersion = versionInfo.FileVersion
		let productVersion = versionInfo.ProductVersion
		
		
		orderby fileVersion descending,file
		select new{Folder= folder,Path=Path.GetDirectoryName(fileFullPath),File=file,fileVersion,productVersion };
		
		q.Dump();
