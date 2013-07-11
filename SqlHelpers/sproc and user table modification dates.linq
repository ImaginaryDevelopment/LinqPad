<Query Kind="SQL">
  <Connection>
    <ID>7675557a-90cf-4ac7-ba6a-fed97efa139b</ID>
    <Persist>true</Persist>
    <Server>jaxpdobe1.payformance.net\backend1</Server>
    <Database>PaySpanConfig</Database>
  </Connection>
</Query>

select 
	*
	from sys.objects
	where is_ms_shipped=0
	and type!='PK'
	order by 1 desc