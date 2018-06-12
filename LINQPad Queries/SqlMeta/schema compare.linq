<Query Kind="FSharpProgram">
  <Reference>C:\tfs\practicemanagement\trunk\bin\Pm.Dal.dll</Reference>
  <Reference>C:\tfs\practicemanagement\trunk\bin\Pm.Schema.dll</Reference>
</Query>

module Helpers = 
    let tee f x = f x; x
    //let (|NonEmptyList|_|) = function | [] -> None | x -> Some x
    type Path with
        static member combine1 y x = Path.Combine(x,y)
    type Directory with
        static member existsOrCreate x = 
            if Directory.Exists x then 
                x
            else
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
type TextMisMatch = {TextPath:string; Text:string; DbText:string}

type SprocMisMatch = 
    | NotFound of name:string*path:string
    | Different of TextMisMatch
    with
        member x.ToDump() = getUF x |> fun (case, f) -> case.Name, f
            
        
        override x.ToString() = 
            getUF x
            |> fun (case,f) -> sprintf "%s:%A" case.Name f
open Pm.Schema.BReusable
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
                        //x.Dump("diff?")
                        x
                        |> Different 
                        |> Some
            with | :? SqlException as ex when ex.Message.Contains(" does not exist in database ") ->
                Some (NotFound(name,fp))
        )
        |> List.ofSeq
    // take a schemas' mismatches and create out the files/directories needed for a diff app to be the ui to the user
    let prepMismatchDiff schemasPath schema (mismatches:TextMisMatch list) : unit =
            List.fold(
                    fun (tempDirOpt:_ option) (schema,_,x) ->
                        let withTemp =
                            // create a subfolder for the schema in a temp path
                            let existsOrCreateSchemaSubDir = 
                                Path.combine1 schema
                                >> Directory.existsOrCreate
                            function
                            | None -> 
                                Util.ClearResults()
                                makeTemp()
                            | Some mmt -> mmt
                            >> (fun x ->
                                x, existsOrCreateSchemaSubDir x.FileRoot, existsOrCreateSchemaSubDir createSchemaSubDir x.DbRoot
                            )
                        match x with
                        // no mismatches for this schema
                        | None  
                        | Some [] -> x
                        | Some mismatches ->
                            let mmt,fr,dr = withTemp tempDirOpt
                            let frp x,drp x= fr |> Path.combine1 x, dr |> Path.combine1 x
                            misMatches
                            |> List.iter(
                                ignore
                            )
                )  None
            
            >> Option.iter(fun x ->
                x.Dump()
            )
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
        let schemaFolders =
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
            |> Option.bind(fun x -> getDirs x |> Option.map(fun dirs -> x, List.ofArray dirs))
            |> function
                // found the Schemas folder, but it had nothing in it
                | Some(_, []) -> None
                | None -> None
                | x -> x
        schemaFolders
        |> Option.map (fun (schemasPath,l) ->
            schemasPath,l
            |> List.choose(
                function 
                |Dir "Programmability" pp as x -> Some (Path.GetFileName x, pp)
                | _ -> None
            ))
        |> Option.map(Tuple2.mapSnd (List.map(fun (schema, progPath) ->
            match progPath with
            | Dir "Stored Procedures" spp ->
                getSprocMisMatches spp (fGetDbText schema)
                |> function
                    | [] -> None
                    | x -> Some x
            | _ -> None
            |> fun x -> (schema, progPath, x)
        )))
        |> fun x -> x   
        |>  Option.tee(
                    function 
                    | schemasPath,(schema,_,None) -> ()
                    | schemasPath,(schema,_,Some []) -> ()
                    | schemasPath,(schema,_,mismatches) -> prepMismatchDiff schemasPath schema mismatches
        )
        

open SchemaCompare
module SchemaDb =
    open Pm.Dal.AdoHelper
    let cn = Pm.Dal.AdoHelper.Connections.Connector.CreateCString dc.Connection.ConnectionString
    let fGetDbText name = 
        Pm.Dal.AdoHelper.getReaderArray cn {CommandText=sprintf "sp_helptext"; CommandType = CommandType.StoredProcedure;OptParameters = Some <| dict [ "@objname", box name] } (fun r ->
            try
                let x = r.GetString 0
            
                string x
            with ex ->
                ex.Dump(sprintf "Failed to get %s" name)
                null
        )
        |> StringHelpers.delimit Environment.NewLine

Util.Cache((fun () -> Util.ReadLine("Path ?")),"schema folder path")
|> validateSqlSchema (fun schema name -> sprintf "%s.%s" schema name |> SchemaDb.fGetDbText )
|> Dump
|> ignore