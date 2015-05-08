<Query Kind="Expression">
  <Connection>
  </Connection>
</Query>

//select sh.name as [Schema],o.name,p.type,p.state, p.state_desc AS permission_state_desc,
// type_desc, o.modify_date,o.create_date
//from    sys.all_objects o
//join sys.schemas sh on o.schema_id=sh.schema_id
//left join sys.database_permissions p
//on p.Major_id=o.object_id -- and  p.type='EX' and p.state='g'
//--left JOIN sys.database_principals dp
//--on     p.grantee_principal_id = dp.principal_id
//
//where sh.name='project' and  o.type in('fn','if','p','x','tf','pc') and  (p.grantee_principal_id is null)
//
//order by modify_date desc

(from o in sys.All_objects.Where (ob => ob.Is_ms_shipped==false && ob.Type=="P")
where o.Name.Contains("final")
join s in sys.Schemas on o.Schema_id equals s.Schema_id
let sPerms= sys.fn_my_permissions(s.Name,"SCHEMA")
let perms= sys.fn_my_permissions(s.Name+"."+o.Name,"OBJECT")
orderby s.Name,o.Name descending

select new{Schema=s.Name, o.Name,o,sPerms, perms })
.ToArray().Select (m => new{m.Schema,m.Name,m.o,m.sPerms, perms=m.perms.Select (p => p.Permission_name)}).Where (m => m.Schema.IsIgnoreCaseMatch("project"))
