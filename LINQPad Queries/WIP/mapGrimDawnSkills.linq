<Query Kind="FSharpExpression" />

let root = @"D:\Projects\GrimModTools\data\"
let combine y x = Path.Combine(x,y)
let dirDirs1 pattern path = Directory.EnumerateDirectories(path,pattern)
let dirFiles1 pattern path = Directory.EnumerateFiles(path,pattern)
let notEmpty = function | [] -> false | _ -> true
let contains (d:string)(x:string) = x.Contains(d)
let toMap f = Seq.map(fun x -> x, f x)
let mapSnd f = Seq.map(fun (x,y) -> x, f y)

let allDirs = Directory.EnumerateDirectories(root) |> Seq.map(combine "records\\skills")
let sortByNewest x = not <| contains "out2" x, not <| contains "out1" x
let readClassTree =
    dirFiles1 "_classtree_class*.dbr"
    >> Seq.collect (File.ReadLines >> Seq.truncate 2>>List.ofSeq)
    >> List.ofSeq
//let root = @"D:\Projects\GrimModTools\data\out\records\skills"
//let classFileMap = dict([
//        1,"Soldier"
//        2,"
//    
//    ] |> List.map(sprintf"playerclass0%i")
let playerSkills = 
    allDirs
    |> Seq.map(dirDirs1 "playerclass*")
    |> Seq.collect id
    |> Seq.groupBy Path.GetFileName
    |> Seq.map(fun (x,items) -> x, items |> Seq.sortBy sortByNewest)
    |> List.ofSeq
playerSkills
|> mapSnd(toMap readClassTree)
//let classMap =
//    playerSkills
//    |> List.map(fun x-> 
//        Directory.EnumerateFiles(x,"*.dbr", SearchOption.AllDirectories) 
//        |> Seq.map(fun file ->
//            file,File.ReadLines file
//            |> Seq.mapi(fun i x -> i,x)
//            |> Seq.choose(fun (i,line) ->
//                    if line.IndexOf("necro",StringComparison.InvariantCultureIgnoreCase) >= 0 then Some(i,line) else None
//            )
//            |> List.ofSeq
//            |> fun x -> x
//        )
//        |> fun f -> x,f
//    )
//    |> fun x -> x
//    |> List.filter(snd>>Seq.exists(snd>>notEmpty))
//    |> List.map(fun x -> x,Directory.EnumerateFiles(x,"*.dbr",SearchOption.AllDirectories) |> Seq.map()
    
//classMap
playerSkills

