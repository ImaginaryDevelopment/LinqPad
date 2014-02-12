<Query Kind="SQL" />

select 
	*
	from sys.objects
	where is_ms_shipped=0
	and type!='PK'
	order by 1 desc