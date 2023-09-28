<Query Kind="SQL" />

select *, dateadd(hh, datediff(hh, getutcdate(), getdate()), auditdate) as local_time_zone from auditlog
order by auditdate desc