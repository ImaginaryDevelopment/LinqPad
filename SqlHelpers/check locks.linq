<Query Kind="SQL" />

select cmd,* from sys.sysprocesses
where blocked > 0

SELECT * FROM sys.dm_tran_locks