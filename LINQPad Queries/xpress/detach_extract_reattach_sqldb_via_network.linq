<Query Kind="FSharpProgram">
  
  <Reference>&lt;RuntimeDirectory&gt;\System.IO.Compression.FileSystem.dll</Reference>
</Query>

let dc = new TypedDataContext()
let dumpt (t:string) x = x.Dump(t); x
let dbName = "PmMigration"
let dbs = 
    dc.sp_databases().Tables.[0].Rows
    |> Seq.cast<DataRow> 
    |> Seq.map (fun dr -> dr.["Database_Name"] |> string) 

// if db exists, detach with drop connections
if dbs |> Seq.exists(fun s -> s=dbName) then

    try
        dc.ExecuteCommand(sprintf "alter database %s SET SINGLE_USER WITH ROLLBACK IMMEDIATE" dbName) |> dumpt "set single user" |> ignore<int>
    with ex when ex.Message.StartsWith(sprintf "User does not have permission to alter database '%s', the database does not exist," dbName) ->
        ex.Message.Dump("might already be detached")
    
    try
        dc.ExecuteCommand(sprintf "sp_detach_db @dbname='%s', @skipchecks=true ;" dbName) |> dumpt "sp_detach_db" |> ignore<int>
    with ex  when ex.Message.StartsWith(sprintf "The database '%s' does not exist. Supply a valid database name. To see available databases, use sys.databases." dbName) ->
        ex.Message.Dump("db already detached or does not exist")
    
let targetFile = @"f:\1.4backup.zip"

let extractFiles targetZip targetPath = 
    System.IO.Compression.ZipFile.ExtractToDirectory(targetZip, targetPath)

let backupTargetDir = Path.Combine(Path.GetDirectoryName targetFile, "unpacked") //Path.GetFileNameWithoutExtension targetFile + ".bak")
backupTargetDir.Dump("backupTargetDir")
extractFiles targetFile backupTargetDir

Debug.Assert(Directory.Exists backupTargetDir)

// delete existing dbFiles to clean up for restore from backup


let attachDb machineRelativePath =
    dc.ExecuteCommand 
        (sprintf """
                CREATE DATABASE %s 
                ON (FILENAME = '%s.mdf'), -- Main Data File .mdf
                (FILENAME = '%s.ldf'), -- Log file .ldf
                --(FILENAME = 'FilePath\SecondaryDataFile.ndf)  -- Optional - any secondary data files
                FOR ATTACH 
                GO""" dbName machineRelativePath machineRelativePath) |> dumpt "attach" |> ignore