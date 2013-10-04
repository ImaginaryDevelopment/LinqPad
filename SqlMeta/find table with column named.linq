<Query Kind="SQL" />


select s.name as schemaName,t.name as TableName,'select distinct '+c.name+' from '+s.name+'.'+t.name as distincter, c.is_nullable,c.is_identity,c.is_computed, t.type_desc, t.type, t.schema_id, t.modify_date  from sys.tables t
	join sys.columns c 
		on c.object_id = t.object_id
	join sys.schemas s on t.schema_id = s.schema_id
	where c.name='PROJECT_NAME'

