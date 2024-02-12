<Query Kind="SQL" />

-- https://www.sqlservercentral.com/scripts/create-database-snapshot-dynamically
/**********************
* Title:Create Database Snapshot
* Date:2016-11-01
* Author:Joe McDermott
* Link:https://msdn.microsoft.com/en-us/library/ms175876(v=sql.110).aspx
* Notes:This script will generate SQL based on the source database and tagert appended name in order to generate a database snapshot.
*Please read comments at the end of variables. Debug is on by default.
* Amend:2016-11-17, updated with dmv SQL and help given from Budd on SQL Server Central. Changed file path to be taken from sys.master_files. 
***********************/
DECLARE  @SourceDatabase varchar(128)= 'GMR_TEST'-- Name of the database you want to snapshot from.
,@SnapshotAppend varchar(128)= 'Snap_20161001'-- Add here what you want to append to the database name for the snapshot. (Example: Snap_20161001)
,@FilePath varchar(200)= NULL-- Edit if you want the snapshot to reside somewhere else. (Example: 'C:\Override\Path\')
,@FileSql varchar(3000)= '' -- Leave blank.
,@SnapSql nvarchar(4000)
,@Debug bit= 1
,@SSName varchar(max)

set @SSName = '[' + @SourceDatabase + '_' + @SnapshotAppend + ']'

IF DB_ID(@SourceDatabase) IS NULL
RAISERROR('Database doesn''t exist. Please check spelling and instance you are connected to.',1,1)

--==================================
-- 1) Set the file path location of the snapshot data files.
--==================================

IF @FilePath = ''
SET @FilePath = NULL

--==================================
-- 2) Dynamicly build up a list of files for the database to snapshot.
--==================================

SELECT @FileSql = @FileSql +
CASE -- Case statement used to wrap a comma in the right place.
WHEN @FileSql <> '' 
THEN + ','
ELSE ''
END + '
( NAME = [' + mf.name + '], FILENAME = ''' + ISNULL(@FilePath, LEFT(mf.physical_name,LEN(mf.physical_name)- 4 ) ) + '_' + @SnapshotAppend + '.ss'')'
-- Remove file extension .mdf, .ndf, and add .ss
FROM sys.master_files AS mf
INNER JOIN sys.databases AS db ON db.database_id = mf.database_id
WHERE db.state = 0 -- Only include database online.
AND mf.type = 0 -- Only include data files.
AND db.[name] = @SourceDatabase

--==================================
-- 3) Build the create snapshot syntax.
--==================================
SET @SnapSql =
'
CREATE DATABASE [' + @SourceDatabase + '_' + @SnapshotAppend + ']
    ON ' 
+ @FileSql +
'
    AS SNAPSHOT OF ['+ @SourceDatabase + '];'

--==================================
-- 4) Print or execute the dynamic sql.
--==================================
IF (@Debug = 1)
BEGIN
PRINT @SnapSql
END
ELSE
BEGIN
print @SSName
EXEC sp_executesql @stmt = @SnapSql
END
GO