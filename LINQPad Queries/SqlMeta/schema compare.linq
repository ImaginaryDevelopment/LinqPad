<Query Kind="FSharpProgram">
  <Reference>C:\tfs\practicemanagement\trunk\bin\Pm.Dal.dll</Reference>
  <Reference>C:\tfs\practicemanagement\trunk\bin\Pm.Schema.dll</Reference>
  <NuGetReference>FSharp.Core</NuGetReference>
</Query>

// take a sqlproj/dbproj read all the sprocs/tables and compare them

// basic functionality appears to work!
// incomplete feature: external diff enabling
//      design:comparison is copying all the files and db-reflected info into a temp folder
//      problem: external diff enabling temp dir isn't copying completely
// unimplemented feature: table diffing

let dc = new TypedDataContext()
let debug = false
let inline dprintn x = if debug then printfn "%s" x
let dDump x = if debug then Dump x; x

module Helpers = 
    let tee f x = f x; x
    let (|ValueString|NonValueString|) x =
        if String.IsNullOrWhiteSpace x then NonValueString x
        else ValueString x
    let (|EndsWithI|_|) d =
        function
        |NonValueString _ -> None
        |ValueString x -> if x.EndsWith(d,StringComparison.InvariantCultureIgnoreCase) then Some x else None
    //let (|NonEmptyList|_|) = function | [] -> None | x -> Some x
    type Path with
        static member combine1 y x = Path.Combine(x,y)
    type Directory with
        static member existsOrCreate x = 
            if Directory.Exists x then 
                x
            else
                dprintn <| sprintf "Creating directory %s" x
                if x.EndsWith(".sql") then invalidOp "this appears to be a file not a directory"
                Directory.CreateDirectory x |> ignore<DirectoryInfo>
                x
    let getUF (x:'a) =
        match Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(x,typeof<'a>) with
        | case, f -> case,f
        
    module Option = 
        // not a map, a side effect
        let tee f = 
            function
            | Some x -> f x; x |> Some
            | None -> None                
    let getUcn x =
        getUF x
        |> fst
        |> fun x -> x.Name
open Helpers

type MisMatchTemp = {TempRoot:string; DbRoot:string; FileRoot:string;}
let makeTemp () = 
    let mmt = 
        let x = Path.Combine(Path.GetTempPath(), Path.GetFileName Util.CurrentQueryPath)
        {TempRoot= Directory.existsOrCreate x;DbRoot = x |> Path.combine1 "Db" |> Directory.existsOrCreate; FileRoot= x |> Path.combine1 "SchemaFiles" |> Directory.existsOrCreate}
    mmt



type CN = Pm.Dal.AdoHelper.Connections.Connector
type SchemasPath = SchemasPath of string 
    with 
        override x.ToString() = match x with | SchemasPath x -> sprintf  "%A" x
        member x.ToDump() = string x
type TextMisMatch = {TextPath:string; Text:string; DbText:string}

type SprocMisMatch = 
    | NotFound of name:string*path:string
    | Different of TextMisMatch
    with
        member x.ToDump() = getUF x |> fun (case, f) -> case.Name, f
            
        
        override x.ToString() = 
            getUF x
            |> fun (case,f) -> sprintf "%s:%A" case.Name f
open Schema.BReusable
module SchemaCompare =
    open System.IO
    
    let getSprocMisMatches sprocFolderPath fGetDbText : SprocMisMatch list =
        let clean = StringHelpers.replace " " "" >> StringHelpers.replace "\r" "" >> StringHelpers.replace "\n" "" >> StringHelpers.replace "\t" ""
        Directory.GetFiles(sprocFolderPath,"*.sql")
        |> Seq.choose(fun fp ->
            let text = File.ReadAllText(fp)
            let cleaned = clean text
            let hash = cleaned.GetHashCode()
            // going to use filename as proxy for sproc name
            // not accounting for file name not matching the create procedure name
            let name = Path.GetFileNameWithoutExtension fp |> fun x -> if x.EndsWith(".proc") then Path.GetFileNameWithoutExtension x else x
            try
                match fGetDbText name with
                | null -> Some (NotFound (name,fp))
                | dbText -> 
                    let dbCleaned = clean dbText 
                    if hash = dbCleaned.GetHashCode() || cleaned |> StringHelpers.stringEqualsI dbCleaned then
                        None
                    else 
                        let x = {TextPath=fp;Text=text; DbText=dbText}
                        x
                        |> Different 
                        |> Some
            with | :? SqlException as ex when ex.Message.Contains(" does not exist in database ") ->
                Some (NotFound(name,fp))
        )
        |> List.ofSeq
    // take a schemas' mismatches and create out the files/directories needed for a diff app to be the ui to the user
    let prepMismatchDiff (SchemasPath schemasPath) schema (mismatches:SprocMisMatch list) : unit =
        mismatches
        |> List.fold(fun (tempDirOpt:_ option) smm ->
            
            let withTemp =
                // create a subfolder for the schema in a temp path
                let existsOrCreateSchemaSubDir = 
                    try
                        Path.combine1 schema
                        >> Directory.existsOrCreate
                    with _ -> 
                        eprintfn "Failing existsOrCreateSchemaSubDir"
                        reraise()
                function
                | None -> 
                    makeTemp()
                | Some mmt -> mmt
                >> (fun x ->
                    x, existsOrCreateSchemaSubDir x.FileRoot, existsOrCreateSchemaSubDir x.DbRoot
                )
            // the path to this schema's temp folder for (f)ile version or (d)atabase verion
            let mmt,fr,dr = withTemp tempDirOpt
            dprintn <| sprintf "fr=%s,dr=%s" fr dr
            
            // if the dbProj version doesn't exist, create the file
            let frp x = 
                let path = fr |> Path.combine1 x
                try
                    File.WriteAllText(path,"")
                with _ ->
                    eprintfn "Bad frp:%s" path
                    reraise()
                path
            match smm with
            // not found in the db
            // create empty file in db reflect (drp)
            // copy dbProj file
            | NotFound(name,path) -> 
                let targetPath = 
                    let filename =
                        path
                        |> Path.GetFileName
                    try
                        filename
                        |> frp
                    with _ ->
                        eprintfn "TargetPath is bad:%s, (%s,%s)" filename name path
                        reraise()
                dprintn <| sprintf "Not found name=%s,path=%s, targetPath=%s" name path targetPath
                try
                    File.Copy(path,targetPath,overwrite=true)
                with _ ->
                    (name,path,targetPath).Dump("copy failed")
                    reraise()
                dprintn <| sprintf "Copied %s to %s" path targetPath
            | Different tmm ->
                
                
                ()
            ()
                
            Some mmt    
        )  None
        |> ignore<_ option>
    type SchemaFolder = {Schema:string; ProgPath:string}
    // SPP the path to the Stored Procedures folder, contains zero to many sprocs
    type SprocFolder = {Schema:string; SPPath:string}
    type SchemaCluster = {SprocFolder:SprocFolder; SprocMismatches: SprocMisMatch list}
    // expects a path that contains a /Schema\.{d}+/ folder which contains the 'Schema Objects' folder
    let validateSqlSchema (fGetDbText:string -> string -> string) path =
        let inline getDirs x = 
            if Directory.Exists x then
                Directory.GetDirectories x
                |> Some
            else None
        let (|Dir|_|) (target:string) (parent:string) =
            let cp = Path.Combine(parent,target)
            if Directory.Exists cp then Some cp else None
        let (|DirMatch|_|) f (parent:string) = getDirs parent |> Option.bind(Seq.tryFind f)

        // allowed path targets:
        // %path%/Schema.*/SchemaObjects/Schemas
        // %path%/Schema Objects/Schemas
        // %path%/Schemas
        //path to the folder containing the individual schemas
        let schemasPath,schemas =
            let isSchemaFolder = StringHelpers.startsWithI "Schema."
            match path with
            | DirMatch isSchemaFolder (Dir "Schema Objects" (Dir "Schemas" x)) ->
                // the Schemas folder
                Some x
            | Dir "Schema Objects" (Dir "Schemas" x) ->
                // the Schemas folder
                Some x
            | Dir "Schemas" x -> Some x
            | _ -> None
            |> Option.bind(fun x -> getDirs x |> Option.map(fun dirs -> SchemasPath x, List.ofArray dirs))
            |> function
//                // found the Schemas folder, but it had nothing in it
//                | Some(_, []) -> None
                | None -> failwithf "Could not find the schemas path"
                | Some x -> x

        schemas
        |> List.choose(
                function 
                |Dir "Programmability" pp as x -> Some {Schema = Path.GetFileName x;ProgPath= pp}
                | _ -> None
        )
        |> List.choose(fun sf ->
                match sf.ProgPath with
                | Dir "Stored Procedures" spp -> 
                    let spp = {Schema=sf.Schema;SPPath=spp}
                    let x = getSprocMisMatches spp.SPPath (fGetDbText sf.Schema)
                    {SprocFolder =  spp; SprocMismatches= x}
                    |> Some
                | _ -> None
        )
        |> fun x -> 
            // clear all the diagnostic db noise
            Util.ClearResults()
            x   
        |> tee (List.iter(fun schemaCluster ->
            prepMismatchDiff schemasPath schemaCluster.SprocFolder.Schema schemaCluster.SprocMismatches
        ))
        

open SchemaCompare
module SchemaDb =
    open Pm.Dal.AdoHelper
    let cn = Pm.Dal.AdoHelper.Connections.Connector.CreateCString dc.Connection.ConnectionString
    let fGetDbText name = 
        let nameForQuery =
            match name with
            | EndsWithI ".sproc" (Before ".sproc" realName) ->
                realName
            | _ -> name
        Pm.Dal.AdoHelper.getReaderArray cn {CommandText=sprintf "sp_helptext"; CommandType = CommandType.StoredProcedure;OptParameters = Some <| dict [ "@objname", box nameForQuery] } (fun r ->
            try
                let x = r.GetString 0
                string x
            with ex ->
                ex.Dump(sprintf "Failed to get %s" name)
                null
        )
        |> StringHelpers.delimit Environment.NewLine
let schemaPath =
    Util.GetPassword("Schema folder path")
schemaPath
|> validateSqlSchema (fun schema name -> sprintf "%s.%s" schema name |> SchemaDb.fGetDbText )
|> Dump
|> ignore