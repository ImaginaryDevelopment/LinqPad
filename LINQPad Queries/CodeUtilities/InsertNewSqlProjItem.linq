<Query Kind="FSharpProgram" />

// add a new item to a .sqlProj
// duplicate-ish of \xpress\create sproc and add to sqlproj.linq *sad face*

// not accounting for the desire to have like-relative-paths near each other
let (|Trim|) =
    function 
    | null | "" -> null
    | x -> x.Trim()
let (|StartsWith|_|) (d:string) =
    function
    | null | "" -> None
    |x when x.StartsWith d -> Some ()
    | _ -> None
let (|Contains|_|) (d:string) =
    function
    | null | "" -> None
    | x when x.Contains(d) -> Some ()
    | _ -> None
let (|Tag|_|) =
    function
    |null -> None
    | Trim (StartsWith "<" _ as x) -> 
        Some x
    | _ -> None
let (|IsItemGroup|_|) =
    function
    |Tag (StartsWith "<ItemGroup") as x -> 
        printfn "Found a tag %s" x
        Some x
    | _ -> None
let (|IsItemClose|_|) =
    function
    | Tag (StartsWith "</ItemGroup>") as x -> Some x
    | _ -> None
let (|IsBuildItem|_|) =
    function
    | Trim (StartsWith "<Build" as x) -> if not <| x.StartsWith "<BuildC" then Some x else None
    | _ -> None
let (|IsFolderInclude|_|) =
    function
    |Trim (StartsWith"<Folder" as x) -> Some x
    | _ -> None
let areDirEqual x y =
    match x, y with
    | "",_ | null,_ | _,"" | _, null -> false
    | _ -> x.TrimEnd('\\') = y.TrimEnd('\\')
    

module Tfs = 
    let path = @"C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\Common7\IDE\CommonExtensions\Microsoft\TeamFoundation\Team Explorer\TF.exe"
    let add p =
        let args = sprintf "add \"%s\"" p
        Util.Cmd(path, args,true)

type ProjectFullPath = |ProjectFullPath of string with member x.Value = match x with |ProjectFullPath y -> y
type BuildItemFullPath = | BuildItemFullPath of string with member x.Value = match x with |BuildItemFullPath y -> y

type State = {InItemGroup:bool;InFolderGroup:bool;PassedFolderGroup:bool;InsertDone:bool; ReversedLines:string list}

let getRelativePath (ProjectFullPath projectFullPath) (BuildItemFullPath itemFullPath) =
    // this doesn't include a trailing '\'
    let projDirectoryPath = Path.GetDirectoryName projectFullPath
    let parentPathLength =  projDirectoryPath |> fun x -> x.Length
    // -1 so this doesn't include a trailing '\'
    let commonRoot = itemFullPath.[0.. parentPathLength - 1]
    if not <| areDirEqual (Path.GetDirectoryName projectFullPath) commonRoot then
        (commonRoot,itemFullPath,Path.GetDirectoryName projectFullPath).Dump("failing")
        failwithf "item path '%s' was not under projectFullPath '%s'" commonRoot projectFullPath
    let rel = itemFullPath.[projDirectoryPath.Length + 1 ..]
    if not <| rel.StartsWith("Schema") then
        (commonRoot,itemFullPath,Path.GetDirectoryName projectFullPath).Dump("failing")
        failwithf "Bad relative path generated %s" rel
    rel
    
let makeBuildItem biFp projPath =
    let relPath = getRelativePath projPath biFp
    [
        sprintf "    <Build Include=\"%s\">" relPath
        "      <QuotedIdentifier>On</QuotedIdentifier>"
        "      <AnsiNulls>On</AnsiNulls>"
        "      <SubType>Code</SubType>"
        "    </Build>"
    ]
let insertBuildItem (BuildItemFullPath bif as bi) projPath text =
    let itemName = Path.GetFileNameWithoutExtension bi.Value
    text
    |> Array.fold(fun state line ->
        let appended = line::state.ReversedLines
        match state, line with
        | _, IsBuildItem (Contains itemName) -> failwithf "this item '%s' already exists at %s" itemName line
        // happy/proceed path
        | {InItemGroup=true;PassedFolderGroup=true;InsertDone=false}, IsBuildItem x ->
            let biLinesRev = List.rev <| makeBuildItem bi projPath
            let lines = line::biLinesRev@state.ReversedLines
            printfn "Doing insert before %s" line
            {state with InsertDone=true;ReversedLines=lines}
        | {InsertDone=true},_ ->
            {state with ReversedLines = appended}
        | {InItemGroup=false}, IsItemGroup _ ->
            printfn "Found an ItemGroup"
            {state with InItemGroup=true;ReversedLines=appended}
        | {InItemGroup=true;PassedFolderGroup=false}, IsFolderInclude _ ->
            {state with ReversedLines= appended;InFolderGroup=true}
        | {InItemGroup=true;PassedFolderGroup=false;InFolderGroup=true}, IsItemClose _ ->
            {state with ReversedLines=appended;InItemGroup=false;PassedFolderGroup=true}
        | {InItemGroup=true;PassedFolderGroup=false}, IsItemClose _ ->
            {state with ReversedLines=appended;InItemGroup=false}
        | {InItemGroup=false;InFolderGroup=false;PassedFolderGroup=false;InsertDone=false}, _ ->
            {state with ReversedLines= appended}
        | {InItemGroup=true},_ -> 
            {state with ReversedLines= appended}
        | _ -> (state,line).Dump("failing"); failwithf "idk"
            
            
    ) {InItemGroup=false;ReversedLines=List.empty;InFolderGroup=false;PassedFolderGroup=false;InsertDone=false}
    |> fun x -> x.ReversedLines
    |> List.rev
    

let (ProjectFullPath pp as projPath) = Util.GetPassword("sqlproj full path") |> ProjectFullPath 
if File.Exists pp |> not then
    failwithf "bad sql proj path:%s" pp
let getDefaultFunText schema name =
    [|
        sprintf "CREATE FUNCTION [%s].[%s]" schema name
        "("
        "    @PatientID int"
        ")"
        "RETURNS int"
        "AS"
        "BEGIN"
        "  declare @Id int"
        "  select @Id=id from somewhere"
        "return @Id"
    |]
let getDefaultSprocText schema name =
    [|
        sprintf "CREATE  PROCEDURE [%s].[%s]" schema name
        "    @payerID varchar(10),"
        "    @AppointmentFacilityID int"
        "AS"
        String.Empty
        "BEGIN"
        "    select * from blah"
        "END"
    |]
// does not auto-correct casing
let schemaTarget = Util.ReadLine("Schema?","dbo", suggestions= ["dbo";"Accounts";"Diags"])   
let isFunction = Util.ReadLine("isFunction?",false)
let name = Util.ReadLine("Name?")
let biFullPath =
    // nice to have: populate the autocomplete bank with the folders found under schemas
    let relPath = 
        if isFunction then
            sprintf @"Schema Objects\Schemas\%s\Programmability\Functions\%s.function.sql" schemaTarget name
        else sprintf @"Schema Objects\Schemas\%s\Programmability\Stored Procedures\%s.sproc.sql" schemaTarget name
    Path.Combine(Path.GetDirectoryName projPath.Value,relPath)
    |> BuildItemFullPath
    
let text = File.ReadAllLines pp
text
|> insertBuildItem biFullPath projPath
|> fun x ->
    File.WriteAllLines(projPath.Value, Array.ofList x)
    if not <| File.Exists biFullPath.Value then
        File.WriteAllLines(biFullPath.Value,if isFunction then getDefaultFunText schemaTarget name else getDefaultSprocText schemaTarget name)
        printfn "Created file at %s" biFullPath.Value
        Tfs.add biFullPath.Value
    else Array.empty
|> Dump
|> ignore