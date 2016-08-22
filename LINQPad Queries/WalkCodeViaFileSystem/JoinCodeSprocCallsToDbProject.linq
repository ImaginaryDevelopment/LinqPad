<Query Kind="FSharpProgram" />

// search code for sproc references and check that they exist in the db project
// consider three different joins, one with each as primary to find things that exist in one but not the other
module Settings = 
    let debug = false
    let slnRoot = Path.Combine(Environment.ExpandEnvironmentVariables("%devroot%"),"PracticeManagement","dev","PracticeManagement")
    let startPath = slnRoot
    let sqlProjDirectory = Path.Combine(slnRoot,"Db") //C:\TFS\PracticeManagement\dev\PracticeManagement\Db
    let sqlProjectFile = 
        // let the finding of code references work even if the sql project file doesn't exist
        Lazy (fun () -> Directory.GetFiles(sqlProjDirectory,"*.sqlproj") |> Seq.head)
    let directoryBlacklist:string list = 
        [
            ".nuget"
            ".sonar"
            ".sonarqube"
            ".vs"
            "obj"
            ".vscode"
            "bin"
            "packages"
            "$tf"
            "bin2"
        ]
type System.String with
    static member before (delimiter:string) (x:string)  = x.Substring(0, x.IndexOf delimiter)
    static member after (delimiter:string) (x:string) =  
        match x.IndexOf delimiter with
        | i when i < 0 -> failwithf "after called without matching substring in '%s'(%s)" x delimiter
        | i -> x.Substring(i + delimiter.Length)

module LinqPad = 
    
    let dumpt (t:string) x =
        x.Dump(t)
        x
    let dumpSeqItemsIf f (t:string)  = 
        Seq.map(fun x -> 
            match f x with
            | Some item -> item |> dumpt t |> ignore
            | None -> ()
            x
        )
    let dumpdt (t:string) x = 
        if Settings.debug then
            x |> dumpt t
        else 
            x
    let dumpdtIf (t:string) f x = 
        if Settings.debug && f x then
            x |> dumpt t
        else x
    
        
module String =
    let endsWith delimiter (x:string) = x.EndsWith(delimiter)
    let endsWithI delimiter (x:string) = x.EndsWith(delimiter,StringComparison.InvariantCultureIgnoreCase)
    let contains (delimiter:string) (x:string) = x.Contains(delimiter)
    let containsI (delimiter:String) (x:string) = x.Contains(delimiter,StringComparison.InvariantCultureIgnoreCase)
    
module Seq =
    let any items = 
        items |> Seq.exists(fun _ -> true)
    let anyF items f item = 
        items
        |> Seq.exists(fun childItem -> f item childItem)
    let endsWithAny (items:#seq<string>) (item:string) =
        anyF items (fun (x:string) (y:string) -> x.EndsWith(y)) item
//        items
//        |> Seq.exists(fun childItem -> item.EndsWith(childItem))
    let endsWithAnyI items item = 
        anyF items (fun (x:string) (y:string) -> x.EndsWith(y, StringComparison.InvariantCultureIgnoreCase)) item
        
let uncurry f (x,y) = f x y
let assertThat b = if b then () else failwithf "Assertion failed"
let assertThatMsg b msg = if b then () else failwith msg
module UtilTests = 
    assertThatMsg(["asb"] |> Seq.any) "Seq.Any failed"
    assertThat ("abc" |> Seq.anyF ["ab";"abc"] (fun x y -> x = y))
    assertThat ("abc.fs" |> Seq.endsWithAnyI [".cs";".fs"])

open LinqPad

let rec recurseDirectory fileTypes basePath = 
    
    match IO.Directory.Exists basePath with 
    | true ->
        if Settings.debug then printfn "Searching in %s" basePath
        seq{
            let files = 
                IO.Directory.EnumerateFiles basePath
                |> dumpSeqItemsIf (fun p -> if Settings.debug && String.endsWithI ".fs" p then Some p else None) "FSharp files to search"
                |> Seq.filter (Seq.endsWithAnyI fileTypes)  //(fun p -> fileTypes |> Seq.exists(fun ft -> p.EndsWith ft))
                |> dumpdt "Files found"
            yield! files
            let directoryFiles =
                IO.Directory.EnumerateDirectories basePath
                |> dumpdt "Directories to search"
                |> Seq.filter (Seq.endsWithAnyI Settings.directoryBlacklist >> not)  //(fun d -> blacklist |> Seq.exists(fun blocked -> d.EndsWith(blocked)) |> not)
                |> Seq.map (recurseDirectory fileTypes)
                |> Seq.collect id
            yield! directoryFiles
            }
    | false -> 
        printfn "Failed to find directory:%s" basePath
        Seq.empty
        
type SprocDetail = {LineNumber:int; Line:string}

let findSprocReferences file = 
    let chooseSprocReference sprocDetail = 
        // consider filtering items like this out:  public PcpDataModel(Pm.Dal.AppDal.AppTypeHost.ServiceTypes.UspPcpGetResult r)
        match String.containsI "\"usp" sprocDetail.Line || String.containsI ".usp" sprocDetail.Line with
        | true -> 
            let sprocName = Regex.Match(sprocDetail.Line,@"(:?\""|\.)(usp\w+)", RegexOptions.IgnoreCase).Groups.[2].Value
            if sprocName.EndsWith("Result") then // type provider has types that end with Result
                None
            else
                Some(sprocName, sprocDetail)
        | false -> None
        
    IO.File.ReadLines file
    |> Seq.mapi (fun i l -> {LineNumber=i;Line=l})
    |> Seq.choose chooseSprocReference
    |> List.ofSeq
    |> fun lines -> file,lines
    
// missing from results: dbESeqReadOnly cn (fun db -> db.UspStatementsFirstGet(apptId))
type FilePath = string

type FileReference = {Path:string; Refs: SprocDetail list}
type SprocReference = {SprocName: string; Details: FileReference list}

let sprocsToVerify = 
    recurseDirectory [".cs";".fs"] Settings.startPath
    |> dumpSeqItemsIf (fun p -> if Settings.debug && String.endsWithI ".fs" p then Some p else None) "files"
    |> Seq.map findSprocReferences
    |> Seq.filter (snd >> Seq.any)
    |> Seq.map (fun (p,nameDetailList) -> nameDetailList |> Seq.map (fun (name,nd) -> (p,name,nd))) //{ SprocName = sprocName; Details = [ {Path=p; Refs = [detail]} ] } )
    |> Seq.collect id
    |> Seq.groupBy (fun (_,name,_) -> name)
    |> Seq.map (fun (sprocName,items) -> sprocName, items |> Seq.map (fun (file,_,detail) -> file,detail) |> List.ofSeq)
    |> dumpdt "recursed files"
    
module DbProjects = 
    // checks that there is a .sproc.sql file that exists in the db project's sub folders
    // check that the project file actually includes this sproc, not that it is just sitting in the dir
    // consider: checking the file actually does define the sproc it claims it does
    
    let sprocFileExtension = ".proc.sql"
    let sprocFiles = Lazy (fun () -> recurseDirectory [sprocFileExtension] Settings.sqlProjDirectory |> List.ofSeq)
    let verifyInDbFolder sprocName = 
        sprocFiles.Value
        |> Seq.map (fun fullpath -> fullpath |> String.before sprocFileExtension)
        |> Seq.filter(String.endsWithI sprocName)
        |> Seq.tryHead
        
    let verifyInDbProj sprocName = 
        Settings.sqlProjectFile.Value
        |> File.ReadLines
        |> Seq.mapi (fun i line -> (i,line))
        |> Seq.filter (fun (_,line) -> line.Contains(sprocName,StringComparison.InvariantCultureIgnoreCase))
        |> Seq.map (fun (i,line) -> {LineNumber=i; Line=line})
        |> Seq.tryHead
        
if not <| Directory.Exists Settings.sqlProjDirectory then raise <| DirectoryNotFoundException()

type SprocVerification = { Name:string; DbProjDetail:SprocDetail option; DbFolderPath:string; FilesToDetails: Map<string, SprocDetail list> }
sprocsToVerify
|> Seq.map (fun (k,v) ->k,DbProjects.verifyInDbProj k, DbProjects.verifyInDbFolder k, v)
|> Seq.filter (fun (k, dp, df, v) -> isNull (box dp) || isNull (box df))
|> Dump
