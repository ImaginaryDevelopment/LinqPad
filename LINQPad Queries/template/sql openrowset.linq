<Query Kind="SQL">
  
</Query>



select * 
from openrowset('SQLOLEDB',
-- 'hostname';'username';'pwd',

'select * from applicationdatabase.dbo.claims') as hc


--exec sp_configure 'show advanced options', 1
--reconfigure
--
--exec sp_configure 'ad hoc distributed queries', 1
--reconfigure