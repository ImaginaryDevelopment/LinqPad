<Query Kind="FSharpExpression" />

//public IEnumerable<string> RecurseDirectory(string basePath,IEnumerable<string> folderBlacklist){
let rec recurseDirectory basePath folderBlacklist =
    let rd path = recurseDirectory path folderBlacklist
    if not <| Directory.Exists basePath then
        Seq.empty
    else 
        seq{
            yield! Directory.EnumerateFiles basePath
            for d in Directory.EnumerateDirectories basePath |> Seq.filter(fun di -> folderBlacklist |> Seq.exists (fun b -> di.EndsWith b) = false) do
                yield d
                yield! rd d
        }    
       
let fileEndings= 
    if Util.ReadLine<bool>("Include vs user files?") then
        [".suo";"proj.user"]
    else List.empty
let targetPath = Util.ReadLine("Target Path to blow away?", System.Environment.GetEnvironmentVariable("devroot", EnvironmentVariableTarget.User))
let doNotDescend = ["$tf"]
if not <| Directory.Exists targetPath then
    "could not find path:".Dump(targetPath)
    failwith "Could not find path"
let deleted= ResizeArray()
let endsWithI d (x:string) = x.EndsWith(d, StringComparison.InvariantCultureIgnoreCase)
let (|EndsWithI|_|) d x = if endsWith d x then Some x else None
recurseDirectory(targetPath,doNotDescend)
|> Seq.iter(fun i ->
    try
        match Directory.Exists i, File.Exists i with
        | false, false ->
            i.Dump("skipping")
        | true, _ ->
				if i.EndsWith("bin",StringComparison.InvariantCultureIgnoreCase)
					|| i.EndsWith("obj", StringComparison.InvariantCultureIgnoreCase) then
					//i.Dump("Deleting");
					Directory.Delete(i,true)
					Directory.Exists(i).DumpIf(id,"delete did not error, but still dir exists")
					deleted.Add(i);
        | _, true ->
            ()
    with ex -> ex.Dump(sprintf "Failed to delete:%s" i)
    deleted.Dump("deleted")
        
)

()
(*


begin legacy copy *)
//void Main()

//{
//	//consider cleaning "C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\ItemTemplatesCache" also? then run devenv.exe /setup
//    // similar functionality at ProjectFolderCleaner.linq
//	//clean bin and obj files, .suo and .user
//	var fileEndings= new [] {".suo","proj.user"};
//	if(Util.ReadLine<bool>("Include vs user files?")==false)
//		fileEndings=Enumerable.Empty<string>().ToArray();
//	var targetPath= Util.ReadLine("Target Path to blow away?", System.Environment.GetEnvironmentVariable("devroot", EnvironmentVariableTarget.User));
//	var doNotDescend = new[]{"$tf"};
//	if(System.IO.Directory.Exists(targetPath)==false){
//		"could not find path:".Dump(targetPath);
//		return;
//	}
//	var deleted= new List<string>();
//	foreach(var i in RecurseDirectory(targetPath,doNotDescend))
//	{
//		try
//		{	        
//			var isDir=System.IO.Directory.Exists(i);
//		
//			if(isDir==false && System.IO.File.Exists(i)==false)
//			{
//				i.Dump("skipping");
//				continue;
//			}
//			if(isDir)
//			{
//				if(i.EndsWith("bin",StringComparison.InvariantCultureIgnoreCase)
//					|| i.EndsWith("obj", StringComparison.InvariantCultureIgnoreCase)) //i.Contains("\\bin\\"))
//				{
//					//i.Dump("Deleting");
//					System.IO.Directory.Delete(i,true);
//					System.IO.Directory.Exists(i).DumpIf(a=>a,"delete did not error, but still dir exists");
//					deleted.Add(i);
//				}
//			} else if(fileEndings.Any ( i.EndsWith)) {
//				//i.Dump("Deleting");
//				System.IO.File.Delete(i);
//				System.IO.File.Exists(i).DumpIf(a=>a,"delete did not error, but still file exists");
//				deleted.Add(i);
//			} 
//		}
//		catch (Exception ex)
//		{
//			ex.Dump("failed to delete:"+i);
//		}
//		
//	}
//		deleted.Dump("deleted");
//}
//
//// Define other methods and classes here
//public IEnumerable<string> RecurseDirectory(string basePath,IEnumerable<string> folderBlacklist){
//	if(System.IO.Directory.Exists(basePath)==false)
//		yield break;
//	foreach(var f in System.IO.Directory.GetFiles(basePath))
//	{
//		yield return f;
//	}
//	foreach(var d in System.IO.Directory.GetDirectories(basePath).Where (di => folderBlacklist.Any (b => di.EndsWith(b))==false)){
//		yield return d;
//		foreach(var i in RecurseDirectory(d,folderBlacklist))
//			yield return i;
//	}