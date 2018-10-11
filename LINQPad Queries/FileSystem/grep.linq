<Query Kind="FSharpProgram" />

// searching from a directory inside all files matching a regex, for text matching a regex, returning the files that have a match
// my own grep!
// via: https://fsharpforfunandprofit.com/posts/recursive-types-and-folds-3b/#grep
// gist: https://gist.github.com/swlaschin/137c322b5a46b97cc8be

let debug = false

let (|RegMatch|_|) (r:Regex) x =
    let m = r.Match(x)
    if m.Success then Some m else None
    
let (|RMatch|_|) p x =
    let m = Regex.Match(x,pattern=p)
    if m.Success then
        Some m
    else None
    
type Tree<'LeafData,'INodeData> =
    | LeafNode of 'LeafData
    | InternalNode of 'INodeData * Tree<'LeafData,'INodeData> seq

module Tree = 

    let rec cata fLeaf fNode (tree:Tree<'LeafData,'INodeData>) :'r = 
        let recurse = cata fLeaf fNode  
        match tree with
        | LeafNode leafInfo -> 
            fLeaf leafInfo 
        | InternalNode (nodeInfo,subtrees) -> 
            fNode nodeInfo (subtrees |> Seq.map recurse)

    let rec fold fLeaf fNode acc (tree:Tree<'LeafData,'INodeData>) :'r = 
        let recurse = fold fLeaf fNode  
        match tree with
        | LeafNode leafInfo -> 
            fLeaf acc leafInfo 
        | InternalNode (nodeInfo,subtrees) -> 
            // determine the local accumulator at this level
            let localAccum = fNode acc nodeInfo
            // thread the local accumulator through all the subitems using Seq.fold
            let finalAccum = subtrees |> Seq.fold recurse localAccum 
            // ... and return it
            finalAccum 

    let rec map fLeaf fNode (tree:Tree<'LeafData,'INodeData>) = 
        let recurse = map fLeaf fNode  
        match tree with
        | LeafNode leafInfo -> 
            let newLeafInfo = fLeaf leafInfo
            LeafNode newLeafInfo 
        | InternalNode (nodeInfo,subtrees) -> 
            let newSubtrees = subtrees |> Seq.map recurse 
            let newNodeInfo = fNode nodeInfo
            InternalNode (newNodeInfo, newSubtrees)

    let rec iter fLeaf fNode (tree:Tree<'LeafData,'INodeData>) = 
        let recurse = iter fLeaf fNode  
        match tree with
        | LeafNode leafInfo -> 
            fLeaf leafInfo
        | InternalNode (nodeInfo,subtrees) -> 
            subtrees |> Seq.iter recurse 
            fNode nodeInfo
            
type LineAccumulator<'t> =
    |HappyAcc of 't list
    |ExcludeFile of excludeMatch:string
    with static member IsExclude(x:LineAccumulator<_>) = match x with |ExcludeFile _ -> true | _ -> false
module LineAcc =
    let map f =
        function
        |HappyAcc x -> f x |> HappyAcc
        |ExcludeFile e -> ExcludeFile e
    
/// Fold over the lines in a file asynchronously
/// passing in the current line and line number tothe folder function.
///
/// Signature:
///   folder:('a -> int -> string -> 'a) -> 
///   acc:'a -> 
///   fi:FileInfo -> 
///   Async<'a>
let foldLinesAsync folder acc (fi:FileInfo) = 
    async {
        let mutable acc = acc
        let mutable lineNo = 1
        use sr = new StreamReader(path=fi.FullName)
        while not <| LineAccumulator<_>.IsExclude acc && not sr.EndOfStream do
            let! lineText = sr.ReadLineAsync() |> Async.AwaitTask
            match acc with
            | HappyAcc x ->
                acc <- folder x lineNo lineText
            | ExcludeFile r ->
                acc <- ExcludeFile r
            lineNo <- lineNo + 1
        return acc
    }
    
let asyncMap f asyncX = async { 
    let! x = asyncX
    return (f x)  }
    
/// return the matching lines in a file, as an async<string list>
let matchPattern textNotPattern textPattern (fi:FileInfo):Async<_ list option> = 
    // set up the regex
    let regex = Text.RegularExpressions.Regex(pattern=textPattern,options=Text.RegularExpressions.RegexOptions.IgnoreCase)
    let regNot = Text.RegularExpressions.Regex(pattern=textNotPattern,options=Text.RegularExpressions.RegexOptions.IgnoreCase)
    
    // set up the function to use with "fold"
    let folder results lineNo lineText =
        match lineText with
        | RegMatch regNot m ->
            ExcludeFile m.Value
        | RegMatch regex _ ->
            let result = fi.Name,lineNo, fi.FullName, lineText //sprintf "%40s:%-5i   %s" fi.Name lineNo lineText
            HappyAcc (result :: results)
        // pass through
        | _ -> HappyAcc results
    
    // main flow
    fi
    |> foldLinesAsync folder (HappyAcc [])
    // the fold output is in reverse order, so reverse it
    |> asyncMap (fun x ->
        match x with
        |HappyAcc x ->
            List.rev x
            |> Some
        |ExcludeFile r ->
            if debug then
                printfn "Excluded %s because of line '%s'" fi.FullName r
            None
    )

    
    
let grep filePattern textPattern textNotPattern fileSystemItem =
    let regex = Text.RegularExpressions.Regex(pattern=filePattern)

    /// if the file matches the pattern
    /// do the matching and return Some async, else None
    let matchFile (fi:FileInfo) :Async<_ list option> =
        if regex.IsMatch fi.Name then
            matchPattern textNotPattern textPattern fi
        else
            async {
                return None
            }

    /// process a file by adding its async to the list
    let fFile asyncs (fi:FileInfo) = 
        // add to the list of asyncs
        (matchFile fi) :: asyncs 

    // for directories, just pass through the list of asyncs
    let fDir asyncs (di:DirectoryInfo)  = 
        asyncs 

    fileSystemItem
    |> Tree.fold fFile fDir []    // get the list of asyncs
    |> Async.Parallel             // merge all asyncs into a single async
    |> fun asyncX ->         // choose the Somes (where a file was processed)
        async{
            let! x = asyncX
            return Array.choose id x
        }
    |> asyncMap (Array.toList >> List.collect id)  // flatten array of lists into a single list    
            
// ==============================================
// IO FileSystem as Tree
// ==============================================

module IOFileSystem_Tree = 

    open System
    open System.IO

    type FileSystemTree = Tree<IO.FileInfo,IO.DirectoryInfo>

    let fromFile (fileInfo:FileInfo) = 
        LeafNode fileInfo 

    let rec fromDir (dirInfo:DirectoryInfo) = 
        let subItems = seq{
            let files = 
                try
                    dirInfo.EnumerateFiles() 
                    |> Seq.map fromFile
                with
                    ex -> 
                    ex.Dump()
                    Seq.empty
            yield! files
            let folders =
                try
                    dirInfo.EnumerateDirectories() 
                    |> Seq.map fromDir
                with ex -> 
                    ex.Dump()
                    Seq.empty
            yield! folders
            }
        InternalNode (dirInfo,subItems)
open IOFileSystem_Tree        
let targetDir = Environment.ExpandEnvironmentVariables("%devroot%")// @"."
let currentDir = fromDir (DirectoryInfo(targetDir))  

printfn "CurrentDir: %A" currentDir
//let filePattern = """(?<!\\(obj|debug)\\)\.(cs|fs)\s*$"""
let filePattern = """.\.(vb|cs|fs|xaml)$"""
let wordPattern = """NotifyPropertyChanged"""
let notWordPatternOpt = "SetAndNotify"
currentDir
|> grep filePattern wordPattern notWordPatternOpt // AddParameter ? // DatabaseConfig
|> Async.RunSynchronously    
|> Dump
|> ignore