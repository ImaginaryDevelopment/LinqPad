<Query Kind="FSharpProgram" />

// my own grep!
// via: https://fsharpforfunandprofit.com/posts/recursive-types-and-folds-3b/#grep
// gist: https://gist.github.com/swlaschin/137c322b5a46b97cc8be

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
        while not sr.EndOfStream do
            let! lineText = sr.ReadLineAsync() |> Async.AwaitTask
            acc <- folder acc lineNo lineText 
            lineNo <- lineNo + 1
        return acc
    }
    
let asyncMap f asyncX = async { 
    let! x = asyncX
    return (f x)  }
    
/// return the matching lines in a file, as an async<string list>
let matchPattern textPattern (fi:FileInfo) = 
    // set up the regex
    let regex = Text.RegularExpressions.Regex(pattern=textPattern)
    
    // set up the function to use with "fold"
    let folder results lineNo lineText =
        if regex.IsMatch lineText then
            let result = fi.Name,lineNo, fi.FullName, lineText //sprintf "%40s:%-5i   %s" fi.Name lineNo lineText
            result :: results
        else
            // pass through
            results
    
    // main flow
    fi
    |> foldLinesAsync folder []
    // the fold output is in reverse order, so reverse it
    |> asyncMap List.rev
    
    
let grep filePattern textPattern fileSystemItem =
    let regex = Text.RegularExpressions.Regex(pattern=filePattern)

    /// if the file matches the pattern
    /// do the matching and return Some async, else None
    let matchFile (fi:FileInfo) =
        if regex.IsMatch fi.Name then
            Some (matchPattern textPattern fi)
        else
            None

    /// process a file by adding its async to the list
    let fFile asyncs (fi:FileInfo) = 
        // add to the list of asyncs
        (matchFile fi) :: asyncs 

    // for directories, just pass through the list of asyncs
    let fDir asyncs (di:DirectoryInfo)  = 
        asyncs 

    fileSystemItem
    |> Tree.fold fFile fDir []    // get the list of asyncs
    |> Seq.choose id              // choose the Somes (where a file was processed)
    |> Async.Parallel             // merge all asyncs into a single async
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
let currentDir = fromDir (DirectoryInfo("."))  
let filePattern = """(?<!\\(obj|debug)\\)\.(cs|fs)\s*$"""
currentDir
|> grep filePattern "phantom.exit"  // AddParameter ? // DatabaseConfig
|> Async.RunSynchronously    
|> Dump
|> ignore