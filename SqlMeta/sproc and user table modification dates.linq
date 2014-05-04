<Query Kind="SQL">
</Query>

select 
	*
	from sys.objects o
	where  is_ms_shipped=0
	and 
	modify_date>'2014/02/06' --and
	--type!='PK'
	order by modify_Date desc