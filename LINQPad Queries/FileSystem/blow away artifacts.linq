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
let doNotDescend = ["$tf";"node_modules"]
if not <| Directory.Exists targetPath then
    "could not find path:".Dump(targetPath)
    failwith "Could not find path"
let deleted= ResizeArray()
let endsWithI d (x:string) = x.EndsWith(d, StringComparison.InvariantCultureIgnoreCase)
let (|EndsWithI|_|) d x = if endsWithI d x then Some () else None
let (|Directory|File|NotFound|) x = if Directory.Exists x then Directory x elif File.Exists x then File x else NotFound

recurseDirectory targetPath doNotDescend
|> Seq.iter(fun i ->
    try
        match i with
        | NotFound ->
            i.Dump("skipping")
        | Directory (EndsWithI "\bin")
        | Directory (EndsWithI "obj") ->
			//i.Dump("Deleting");
			Directory.Delete(i,true)
			if Directory.Exists i then
               i.Dump("delete did not error, but still dir exists")
            else
    			deleted.Add(i);
		| Directory _ -> ()
	    | File i when fileEndings |> Seq.exists i.EndsWith ->
	        File.Delete i
	        if  File.Exists i then
	            i.Dump( "delete did not error, but still file exists")
            else deleted.Add i
        | File _ -> ()
		
    with ex -> ex.Dump(sprintf "Failed to delete:%s" i)
)
deleted