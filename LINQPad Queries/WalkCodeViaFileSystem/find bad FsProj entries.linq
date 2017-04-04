<Query Kind="FSharpProgram" />

// get bad fsproj includes
// not sure if subfolder's subfolders not all being in the same place errors
// however this script worked against 1 exception anyhow
open BReusable
let target = @"C:\TFS\PracticeManagement\dev\PracticeManagement\Pm.Web\Pm.Web.fsproj"

let (|StartsWith|_|) (toMatch:string) (x:string) = 
    if not <| isNull x && not <| isNull toMatch && toMatch.Length > 0 && x.StartsWith(toMatch, StringComparison.InvariantCultureIgnoreCase) then
        Some () 
    else None
    
let subfoldersProcessed = ResizeArray<string>()
let mutable currentSubFolder:string = null
let isItemInclude = 
    trim >>
    function 
    |StartsWith "<Compile" 
    |StartsWith "<Content"
    |StartsWith "<None"
        -> true
    | _ -> false
    
let (<||>) f1 f2 x = f1 x || f2 x    

type ItemGroupItem =
    |RootItem of string
    |NestedItem of string
    
let beforeLastDelim (x:string) = 
    //let hasAny = x contains "/" || x contains "\\"
    x.LastIndexOfAny([ '\\';'/'] |> Array.ofList)
    |> fun i -> x.Substring(0,i)
//    |> fun s -> 
//        (s,x).Dump("beforedelim?")
//        s
    
type ReadItemResult = 
    |Success
    |FailedItem of string
    
target 
|> File.ReadAllLines
|> Seq.filter isItemInclude
// items not nested don't have this problem
//|> Dump
|> Seq.map (trim >> after "Include=\"" >> before "\"")
// root items are only observed because if there is a root item between 2 items of the current SubFolder -> exception
|> Seq.map (fun x -> if x |> (String.contains "\\" <||> String.contains "/")  then NestedItem x else RootItem x)
//|> Dump
|> Seq.map(
    function 
    | RootItem x -> 
        if isNull currentSubFolder then
            Success
        else
            // this should never happen if the nested branch is set up properly, eh?
            if subfoldersProcessed |> Seq.contains currentSubFolder then
                FailedItem x
            else
                subfoldersProcessed.Add currentSubFolder
                currentSubFolder <- null
                Success
    | NestedItem x ->
        let itemSubFolder = x |> beforeLastDelim
        match isNull currentSubFolder, subfoldersProcessed |> Seq.contains itemSubFolder with
        // last item was a root item, and the new item's subFolder has not been processed
        | true, false ->
            currentSubFolder <- itemSubFolder
            Success
        // cases:
        // last item was a root item, but the new item's subFolder has already been processed
        // _, subFoldersProcessed already has this one, blow up
        | _, true ->
            FailedItem x
        // current isn't null, and the new item's subFolder hasn't been processed
        | false, false ->
            if itemSubFolder = currentSubFolder then
                Success
            else 
                subfoldersProcessed.Add currentSubFolder
                currentSubFolder <- itemSubFolder
                Success
    )
|> Dump
//|> Seq.filter ()
