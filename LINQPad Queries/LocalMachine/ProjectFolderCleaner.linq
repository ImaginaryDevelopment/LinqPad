<Query Kind="FSharpProgram" />

//consider cleaning "C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\ItemTemplatesCache" also? then run devenv.exe /setup
// similar functionality at blow away artifacts.linq
let lastDir = 
    match System.AppDomain.CurrentDomain.GetData("lastDir") with
    | :? string as x -> x
    | _ -> @"C:\projects"
let target= Util.ReadLine("target?", lastDir )
if Directory.Exists target then
    AppDomain.CurrentDomain.SetData("lastDir", target)
    
let deletePackages=Util.ReadLine<bool>("delete packages?",false)
let searchDirs= [| "_ReSharper*"; "bin"; "obj"; "build" |]
let deleteDirectory s = 
    let inline onFail exMsg = 
        exMsg.Dump("failed to delete: " + s)
        false
	try
		System.IO.Directory.Delete(s,true) |> ignore
		true
    with 
        | :? UnauthorizedAccessException as uae ->
            onFail uae.Message
        | :? DirectoryNotFoundException as ex ->
            onFail ex.Message
        | :? IOException as ioex ->
            onFail ioex.Message
searchDirs
|> Seq.iter (fun searchPattern ->

    let q=System.IO.Directory.GetDirectories(target,searchPattern, SearchOption.AllDirectories);
    let mutable count=0
    q
    |> Seq.iter (fun d -> 
        if deleteDirectory d then
            count <- count + 1
    )
    
    count.Dump("deleted via " + searchPattern);
)
if deletePackages then
//package folders
    let mutable packageCount=0
    
    System.IO.Directory.GetDirectories(target,"packages", SearchOption.AllDirectories)
    |> Seq.iter (fun p ->
        System.IO.Directory.GetDirectories p
        |> Seq.iter (fun d ->
            //delete all subfolders, leaving packages.config and repositories.config alone
            if deleteDirectory d then
                packageCount <- packageCount + 1
        )
    )
    packageCount.Dump("deleted via packages");