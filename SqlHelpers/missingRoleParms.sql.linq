select sh.name as [Schema],o.name,p.state_desc AS permission_state_desc,
 type_desc, o.modify_date,o.create_date
from    sys.all_objects o
join sys.schemas sh on o.schema_id=sh.schema_id
left join sys.database_permissions p
on p.Major_id=o.object_id and p.type='EX' and p.state='g'
--left JOIN sys.database_principals dp
--on     p.grantee_principal_id = dp.principal_id

where sh.name in ('Common','Treasury') and o.type in('fn','if','p','x','tf','pc') and  (p.grantee_principal_id is null)

<Query Kind="SQL">
</Query>

select sh.name as [Schema],o.name,p.state_desc AS permission_state_desc,
 type_desc, o.modify_date,o.create_date
from    sys.all_objects o
join sys.schemas sh on o.schema_id=sh.schema_id
left join sys.database_permissions p
on p.Major_id=o.object_id and p.type='EX' and p.state='g'
--left JOIN sys.database_principals dp
--on     p.grantee_principal_id = dp.principal_id

where  o.type in('fn','if','p','x','tf','pc') and  (p.grantee_principal_id is null)

order by modify_date desc