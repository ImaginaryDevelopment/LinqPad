<Query Kind="FSharpProgram" />

let transform (vsVersion:string) (target:string) =
	if not <| String.IsNullOrEmpty(target) then 
		let targetCwd = 
			Path.GetDirectoryName(target)
			|> Path.GetDirectoryName
			|> Path.GetDirectoryName
		let inflectorPath = Path.Combine(targetCwd,@"packages\Inflector.1.0.0\lib\net45\Inflector.dll")
		inflectorPath.Dump()
		File.Exists(inflectorPath).Dump("inflector exists")
		Environment.CurrentDirectory <- targetCwd
		targetCwd.Dump("new cwd")
	
	
	Util.Cmd(sprintf @"C:\Program Files (x86)\Common Files\microsoft shared\TextTemplating\%s\TextTransform.exe" vsVersion,sprintf "\"%s\"" target)
	|> Dump
	
transform "14.0" @"D:\Users\DBee\My Documents\Visual Studio 2015\Projects\FSharpWeb2015\FSharpWeb2015\Controllers\OData.tt"
	//<| sprintf "\"%s\"" @"D:\Users\DBee\My Documents\Visual Studio 2015\Projects\FSharpWeb2015\FSharpWeb2015\Controllers\OData.tt"