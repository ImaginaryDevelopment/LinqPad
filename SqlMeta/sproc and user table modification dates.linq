<Query Kind="SQL">
</Query>

select 
	*
	from sys.objects o
	where  is_ms_shipped=0
	and 
	(create_date>'2014/05/10' OR modify_date>'2014/05/10')--and
	--type!='PK'
	order by
	 --modify_Date 
	 create_date
	 desc