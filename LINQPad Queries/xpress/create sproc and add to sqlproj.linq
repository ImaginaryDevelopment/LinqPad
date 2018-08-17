<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 14.0\Common7\IDE\Extensions\vkcobwis.wzm\Microsoft.TeamFoundation.Client.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
</Query>

// add a sproc to the project without visual studio
let dc = new TypedDataContext()
module Helpers = 
    let dumpt t x = x.Dump(description=t); x
    let replace (d:string) r (x:string) = x.Replace(d,r)
    let delimit d (x:string seq) = String.Join(d,x)
    let (|RMatch|_|) p x=
        let m = Regex.Match(x,p)
        if m.Success then
            Some m
        else None
    
    module Option =
        let getOrDefault d = function | Some x -> x | None -> d
        
        
open Helpers        

module Tfs = 
    let path = @"C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\Common7\IDE\CommonExtensions\Microsoft\TeamFoundation\Team Explorer\TF.exe"
    let add p =
        let _result = 
            let args = sprintf "add \"%s\"" p
            Util.Cmd(path, args,true)
        ()
    
// nice to have: auto detect existing proc and change to alter instead of create

let sprocText = Util.Cache(Func<_>(System.Windows.Forms.Clipboard.GetText), "sprocText")
sprocText .Dump("sproc text")
let sprocName = 
    match sprocText with
    |RMatch "procedure (\[?dbo\]\.)?\[?(\w+)\]?" m -> m.Groups.[2].Value
    | _ -> failwithf "could not find sproc name in cache or in clipboard. Found:%s" sprocText
let createOrAlter () =
    let sprocText = sprocText //|> replace "create" "create or alter"
    // this doesn't seem to work in our version of sql
    //sprintf "create or alter procedure %s" text |> dc.ExecuteCommand |> ignore<int>
    sprocText |> dc.ExecuteCommand |> ignore<int>

type SprocLine = {Index:int;Line:string;Path:string;} with
    member x.Name = 
        try
            Path.GetFileNameWithoutExtension x.Path
        with ex ->
            x.Dump("name fail!")
            reraise()
type  LineType =
    | RegularLine of string
    | SprocLine of SprocLine 
    
type FoldState = { InsertDone:bool; AccruedLines:string list}
let containsSproc path =
    Seq.exists(function |SprocLine x -> x.Path = path | _ -> false)
let addSprocToSqlProj sqlProjText relPath sqlProj =
    let newSqlProjText = 
        sqlProjText
        |> Seq.fold(fun fs ->
            function
            | RegularLine l -> {fs with AccruedLines = l::fs.AccruedLines}
            | SprocLine x -> 
                if fs.InsertDone then
                    {fs with AccruedLines = x.Line::fs.AccruedLines}
                else 
                    x.Dump("Inserting before")
                    // this must be in reverse order since the fold is accruing in reverse
                    let newText = [
                        sprintf "    <Build Include=\"%s\">" relPath
                        "      <SubType>Code</SubType>"
                        "      <AnsiNulls>On</AnsiNulls>"
                        "      <QuotedIdentifier>On</QuotedIdentifier>"
                        "    </Build>"
                    ] List.rev
                    newText.Dump("new sproc include text")
                    {InsertDone=true; AccruedLines = x.Line :: newText@fs.AccruedLines}
        ) {InsertDone=false;AccruedLines=List.empty}
        |> fun x -> x.AccruedLines
        |> List.rev
        |> Array.ofList
    File.WriteAllLines(sqlProj,newSqlProjText)
// add sproc to sqlProj
(
    let devRoot = Environment.ExpandEnvironmentVariables("%devroot%")
    let (|Classify|) (i:int,x:string) =
        match x with
        | RMatch "<Build .*Include=\"([^\"]+)\"" m ->
            SprocLine {Index=i;Line=x;Path=m.Groups.[1].Value}
        | x -> RegularLine x
           
    System.IO.Directory.EnumerateFiles(devRoot, "*.sqlproj",SearchOption.AllDirectories) |> Seq.tryHead
    |> function 
        | Some sqlProj ->
            let sqlProjText = 
                File.ReadAllLines(sqlProj)
                |> Array.mapi(fun i x -> (i,x))
                // loses the index for regular lines, but not important
                |> Array.map (|Classify|)
            let sprocFolders = sqlProjText |> Array.choose (function | SprocLine x -> Some x | _ -> None) |> Array.map(fun x -> x.Path |> Path.GetDirectoryName) |> Array.distinct |> Array.sort
            let defaultPath = sprocFolders |> Seq.tryFind((=)"Schema Objects\Schemas\dbo\Programmability\Stored Procedures") |> Option.getOrDefault ""
            let desiredPath = Util.ReadLine("TargetPath?", defaultPath,sprocFolders)
            // find the first item with the desired relative path, and insert before it
            let relPath = Path.Combine(desiredPath, sprintf "%s.proc.sql" sprocName)
            let fullTargetPath = Path.Combine(Path.GetDirectoryName sqlProj, relPath) //desiredPath, sprintf "%s.proc.sql" sprocName)
            printfn "Writing to %s" fullTargetPath
            // write out text to sqlProj relative path
            File.WriteAllText(fullTargetPath, sprocText)
            Tfs.add fullTargetPath
            // check if the sproc is already referenced in the sql proj
            if not <| containsSproc relPath sqlProjText then
                addSprocToSqlProj sqlProjText relPath sqlProj
            else printfn ".sqlproj already has the sproc added"
            
        | None -> failwithf "could not locate .sqlproj file in %s" devRoot
        
)

//createOrAlter()