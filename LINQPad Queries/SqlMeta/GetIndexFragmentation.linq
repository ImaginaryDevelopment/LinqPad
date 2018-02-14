<Query Kind="SQL" />

-- index fragmentation

	select --By ImaginaryDevelopment
	s.name as schemaName,o.name as tablename,i.name as indexName,i.is_primary_key,i.fill_factor,  ps.avg_fragmentation_in_percent as  avgFragPercent
	--,i.*, ps.*,s.*
	from sys.dm_db_index_physical_stats(db_id(),null,null,null,null) as ps
		join sys.indexes as i on ps.object_id=i.object_id and ps.index_id=i.index_id
		join sys.objects as o on i.object_id=o.object_id
		join sys.schemas as s on s.schema_id=o.schema_id
		where ps.database_id=DB_ID()
		and ps.avg_fragmentation_in_percent>20
		order by ps.avg_fragmentation_in_percent desc