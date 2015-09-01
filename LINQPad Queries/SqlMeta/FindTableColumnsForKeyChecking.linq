<Query Kind="Expression">
  <Connection>
    <ID>4e94eacc-a31d-4687-947b-e4c9804c895a</ID>
    <Persist>true</Persist>
    <Server>(local)</Server>
    <Database>XPEncounter</Database>
    <ShowServer>true</ShowServer>
    <IncludeSystemObjects>true</IncludeSystemObjects>
  </Connection>
</Query>

from c in sys.Columns
join co in sys.Objects on c.Object_id equals co.Object_id
from tL in sys.Systypes.Where(t => c.User_type_id == t.Type || c.System_type_id == t.Type).DefaultIfEmpty()
//join tLeft in sys.Systypes on c.User_type_id equals tLeft.Type into tL
//join stLeft in sys.Systypes on c.System_type_id equals stLeft.Type into stL
where co.Type =="U"
select new{ c, co,tL}
//from c in this.INFORMATION_SCHEMA.COLUMNS
//					.Where(c =>c.TABLE_CATALOG== this.Connection.Database)
//					.Where(c => c.DATA_TYPE =="int")
//					
//join fkLeft in this.sys.Foreign_key_columns on 
	//this.INFORMATION_SCHEMA.TABLE_CONSTRAINTS on c.COLUMN_NAME equals fkLeft.col
		