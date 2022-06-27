<Query Kind="FSharpExpression" />

let target = Util.Cache(fun () -> 
    let mutable path = null
    while isNull path || not <| Directory.Exists path do
        path <- Util.ReadLine("Where is your Scripts folder?",@"C:\tfs\practicemanagement\trunk\Pm.Web\Scripts")
    path
)
            
let inline stringf format (x : ^a) = 
    (^a : (member ToString : string -> string) (x, format))
    
let targetFileExt = [".jsx";".js"]
let fileNames = 
    targetFileExt
    |> Seq.collect(fun ext ->
        Directory.EnumerateFiles(target,sprintf "*%s" ext, SearchOption.AllDirectories)    
    )
    |> List.ofSeq
    |> List.map(fun fp ->
        Path.GetFileName(fp), File.ReadAllText fp |> hash |> stringf "N0", FileInfo(fp).LastWriteTimeUtc, Path.GetDirectoryName fp
    )


fileNames