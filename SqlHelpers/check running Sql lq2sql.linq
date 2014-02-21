<Query Kind="Expression">
  <Connection>
    <ID>282ab884-278b-4d31-a865-51e85288681f</ID>
    <Persist>true</Persist>
    <IncludeSystemObjects>true</IncludeSystemObjects>
  </Connection>
</Query>

//SELECT s.login_name,d.name as dbName, 
//req.command,
//
//
//sqltext.TEXT,
//s.program_name,
//req.session_id,
//req.status,
//
//req.cpu_time,
//req.total_elapsed_time,
//req.transaction_id,
//s.host_name,
//s.client_interface_name,
//s.memory_usage,
//s.database_id


 from req in sys.Dm_exec_requests
 	let sqltext = sys.dm_exec_sql_text(req.Sql_handle)
	join sl in sys.Dm_exec_sessions on req.Session_id equals sl.Session_id into sLeft
	from s in sLeft.DefaultIfEmpty()
	join dl in sys.Databases on s.Database_id equals dl.Database_id into dLeft
	from d in dLeft.DefaultIfEmpty()
	where d==null || d.Name=="CVS"
	select new {req,sqltext,s,d}