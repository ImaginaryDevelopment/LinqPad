<Query Kind="FSharpProgram">
  
</Query>

// reset db, verify schema expectations
let dc = new TypedDataContext()
let delimit s (items:string seq) = String.Join(s,items)
let dumpt (t:string) x = x.Dump(t); x
type RunType = // new method requires master be selected as the db for restore, while checkrestore must use the target db
    | Restore
    | CheckRestore
let runType = if dc.Connection.Database ="master" then Restore else CheckRestore

module Seq = 
    let any (items: _ seq) = items |> Seq.exists(fun _ -> true)
    let contains (items:_ seq) x = items.Contains(x)
    let containsC comparer (items: _ seq) x = items.Contains(x,comparer)

type BackupPartForRestore = { BackupFilePartName:string;TargetFullPath:string}

type RestoreStrategy = 
    | MyComposed
    // caught in profiler:
    // RESTORE DATABASE [PROD] FROM  DISK = N'E:\prod16\pr16.bak' WITH  FILE = 1,  MOVE N'PR16_PracticeManagement' TO N'C:\Program Files\Microsoft SQL Server\MSSQL10_50.MSSQLSERVER\MSSQL\DATA\PROD.mdf',  MOVE N'PR16_PracticeManagement_log' TO N'C:\Program Files\Microsoft SQL Server\MSSQL10_50.MSSQLSERVER\MSSQL\DATA\PROD.ldf',  NOUNLOAD,  STATS = 10
    | ProfilerCaptured of backupFullPath:string*backupParts:(BackupPartForRestore seq)
    
let restore dbName rs= 
    let killConnections () = // using info from http://stackoverflow.com/questions/11620/how-do-you-kill-all-current-connections-to-a-sql-server-2005-database
        let query = sprintf @"
DECLARE @dbname sysname
SET @dbname = '%s'

DECLARE @spid int
SELECT @spid = min(spid) from master.dbo.sysprocesses where dbid = db_id(@dbname)
WHILE @spid IS NOT NULL
BEGIN
EXECUTE ('KILL ' + @spid)
SELECT @spid = min(spid) from master.dbo.sysprocesses where dbid = db_id(@dbname) AND spid > @spid and @spid <> @@spid
END"
        let query = query dbName
        dc.ExecuteCommand query
        
    let tryGetDbState dbName = 
        dc.ExecuteQuery<string>(sprintf "SELECT DATABASEPROPERTYEX('%s','UserAccess')" dbName) 
        |> Seq.head
    match rs with
        |  MyComposed -> 
            sprintf "use master;\r\n restore database %s" dbName // pmmigration
        | ProfilerCaptured (backupFullPath, backupParts) -> 
            if not <| File.Exists backupFullPath then failwithf "Backupfile not found: %s" backupFullPath
            
            // merging in ideas from http://blog.sqlauthority.com/2007/02/25/sql-server-restore-database-backup-using-sql-script-t-sql/
            let preStart = sprintf @"ALTER DATABASE %s SET SINGLE_USER WITH ROLLBACK IMMEDIATE" dbName
            let starter = sprintf "RESTORE DATABASE [%s] FROM DISK = N'%s' WITH  FILE = 1," dbName backupFullPath
            let partsComposed = backupParts |> Seq.map(fun bp -> sprintf "MOVE N'%s' TO N'%s'" bp.BackupFilePartName bp.TargetFullPath) |> List.ofSeq
            let partsJoined = partsComposed |> delimit ", "
            let finisher = ",  NOUNLOAD,  STATS = 10"
            sprintf "%s\r\n%s%s%s" preStart starter partsJoined finisher
    |> fun text -> 
        tryGetDbState dbName |> dumpt "dbState before restore starts" |> ignore
        killConnections() |> ignore
        try
            try
                (text,dc.ExecuteCommand text).Dump("restore command")
            with 
                |ex when ex.Message.Contains("The database is already fully recovered.") -> 
                    ex.Message.Dump()
                    Util.OnDemand("Full exception", fun () -> ex).Dump()
                |ex -> 
                    text.Dump("restore text")
                    ex.Message.Dump()
                    Util.OnDemand("Full exception", fun () -> ex).Dump()
                    reraise()
        finally 
            match rs with 
            | ProfilerCaptured _ -> 
                try
                    let isMulti_User = tryGetDbState dbName |> (=) "MULTI_USER"
                    if not isMulti_User then
                        dc.ExecuteCommand(sprintf "ALTER DATABASE %s SET MULTI_USER" dbName) |> dumpt "set multi_user result" |> ignore
                with ex -> ex.Dump("set multi_user failed")
            | _ -> ()

// should not exist:

// Charge table (should have charges table, not charge table)
// ChargesTable:
//  CodeUnique,ModCodeId
let checkTableSchema name (expected:_ seq) (unexpected:_ seq) = 
    let columns = dc.ExecuteQuery<string>(sprintf "select name from sys.columns where object_id=object_id('%s')" name) |> List.ofSeq
    //columns.Dump("columns found")
    let ``contains all expected`` () = 
        let missing = expected |> Seq.filter (columns |> Seq.containsC StringComparer.InvariantCultureIgnoreCase >> not) |> List.ofSeq
        match missing with
        | [] -> ()
        | x -> 
            let ex = InvalidOperationException()
            ex.Data.Add("missing", x)
            raise ex
            
    let ``contains no unexpected`` () = 
        let unexpected = columns |> Seq.filter (fun unEColu -> unexpected.Contains(unEColu, StringComparer.InvariantCultureIgnoreCase)) |> List.ofSeq
        match unexpected with
        | [] -> ()
        | x -> 
            let ex = InvalidOperationException()
            ex.Data.Add("unexpected", x)
            raise ex
            
    ``contains all expected`` ()
    ``contains no unexpected`` ()

()
DateTime.Now.Dump("restore started")
match runType with
| Restore ->
    ProfilerCaptured("E:\prod16\pr16.bak", 
                        [
                            {BackupPartForRestore.BackupFilePartName = "PR16_PracticeManagement";TargetFullPath = @"C:\Program Files\Microsoft SQL Server\MSSQL10_50.MSSQLSERVER\MSSQL\DATA\PROD.mdf"}
                            {BackupPartForRestore.BackupFilePartName = "PR16_PracticeManagement_log";TargetFullPath = @"C:\Program Files\Microsoft SQL Server\MSSQL10_50.MSSQLSERVER\MSSQL\DATA\PROD.ldf"}
                        ]
                    )
    |> restore "PROD"
    DateTime.Now.Dump("Finished restore!")
    
| CheckRestore ->
    [
        "dbo.charges", ["CodeID"; "CodeUnique"; "Charge"; "ModCodeId"; "Charge";"TotalCharge";"ChargeDOS"], ["ProcedureCodeId"; "Modifier1"; "Amount";"TotalAmount"]
        "dbo.Payments", ["Amount"; "Timestamp"; "AppointmentId"; "IsReconciled"; "ChargeReconciled"], ["TotalAmount"; "Created"]
        "dbo.Claims",  ["MaxLevel"], ["_MaxLevel_"]
        "dbo.GuarantorProfiles", ["GuarantorProfileID"; "GuarantorProfilePatientID"], ["PayerProfileID"]
    ]
    |> Seq.iter (fun (tn,ex,unExpected) -> checkTableSchema tn ex unExpected)