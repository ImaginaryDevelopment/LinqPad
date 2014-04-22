<Query Kind="SQL" />

--merge statements

/*
Post-Deployment Script Template							
--------------------------------------------------------------------------------------
 This file contains SQL statements that will be appended to the build script.		
 Use SQLCMD syntax to include a file in the post-deployment script.			
 Example:      :r .\myfile.sql								
 Use SQLCMD syntax to reference a variable in the post-deployment script.		
 Example:      :setvar TableName MyTable							
               SELECT * FROM [$(TableName)]					
--------------------------------------------------------------------------------------
http://blogs.msdn.com/b/ssdt/archive/2012/02/02/including-data-in-an-sql-server-database-project.aspx
*/
-- Reference Data for quota_group_log_type_codeset 
use cvs;
go
set identity_insert project.quota_group_log_type_codeset ON
go
merge into project.quota_group_log_type_Codeset as target
using (values 
	(1,'Added Answer'),
	(2, 'Removed Answer'),
	(3, 'Status Change'),
	(4,'Dynamic Recruitment Status'),
	(5,'Added Members'),
	(6, 'Added Quota Group Member Criteria'),
	(7, 'Dynamic Survey Status'),
	(8, 'Deleted Quota Group Member Criteria')
)
		as source (quota_group_log_type_id,quota_group_log_type_name)
		on target.quota_group_log_type_id = source.quota_group_log_type_id

--update matched rows
when matched then
	update set quota_group_log_type_name = source.quota_group_log_type_name

--insert new rows
	when not matched by target then
		insert(quota_group_log_type_id,quota_group_log_type_name)
		values(quota_group_log_type_id,quota_group_log_type_name)

-- delete rows that are in target but not the source
when not matched by source then DELETE
-- what comes out of the merge console
OUTPUT $action, inserted.*, deleted.*;
go
set identity_insert project.quota_group_log_type_codeset OFF