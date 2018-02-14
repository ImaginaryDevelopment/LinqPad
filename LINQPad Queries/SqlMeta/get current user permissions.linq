<Query Kind="SQL" />

SELECT top 100 percent SCHEMA_NAME(schema_id) + '.' + name
	, type_desc
	, HAS_PERMS_BY_NAME(SCHEMA_NAME(schema_id) + '.' + name,
	    'OBJECT', 'SELECT')  AS can_select
	, HAS_PERMS_BY_NAME(SCHEMA_NAME(schema_id) + '.' + name,
	    'OBJECT', 'UPDATE') AS can_update
	, HAS_PERMS_BY_NAME(SCHEMA_NAME(schema_id) + '.' + name,
	    'OBJECT', 'INSERT') AS can_insert
	, HAS_PERMS_BY_NAME(SCHEMA_NAME(schema_id) + '.' + name,
	    'OBJECT', 'DELETE') AS can_delete
	, HAS_PERMS_BY_NAME(SCHEMA_NAME(schema_id) + '.' + name,
	    'OBJECT', 'EXECUTE') AS can_execute
	, create_date, modify_date
	FROM sys.all_objects
	WHERE type_desc IN ('USER_TABLE', 'SQL_STORED_PROCEDURE', 'VIEW')
	AND SCHEMA_NAME(schema_id) NOT IN ('sys', 'INFORMATION_SCHEMA')
	ORDER BY schema_id,type_desc, name;