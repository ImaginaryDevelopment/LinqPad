<Query Kind="SQL">
</Query>

select ss.name as [schema],tt.name as [tableTypeName], c.name,t.name as ColumnType,c.is_nullable,  c.collation_name as collation ,c.max_length,t.max_length as typeMaxLength
	from sys.columns c
	join sys.table_types tt on c.object_id =tt. type_Table_object_id
	join sys.[schemas] ss on tt.schema_id = ss.schema_id
	join sys.types t on t.system_type_id = c.system_type_id and t.user_type_id = c.user_type_id-- or user_type_id?
	order by tt.type_table_object_id,c.column_id