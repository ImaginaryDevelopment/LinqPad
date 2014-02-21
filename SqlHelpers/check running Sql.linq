<Query Kind="SQL">
  <Connection>
    <ID>282ab884-278b-4d31-a865-51e85288681f</ID>
    <Persist>true</Persist>
    <IncludeSystemObjects>true</IncludeSystemObjects>
  </Connection>
</Query>

SELECT s.login_name,d.name as dbName, 
req.command,


sqltext.TEXT,
s.program_name,
req.session_id,
req.status,

req.cpu_time,
req.total_elapsed_time,
req.transaction_id,
s.host_name,
s.client_interface_name,
s.memory_usage,
s.database_id
FROM sys.dm_exec_requests req
CROSS APPLY sys.dm_exec_sql_text(sql_handle) AS sqltext
left join sys.dm_exec_sessions s on req.session_id = s.session_id
left join sys.databases d on s.database_id = d.database_id
where d.name='CVS'
--order by d.name
