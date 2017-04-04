<Query Kind="SQL">
  
</Query>

-- another get schema table column info script


--SELECT column_name as primarykeycolumn
----,object_id(ku.table_schema + '.' + ku.table_name) as tableObjId
--,t.name as TypeName -- *
----tbl.*
---- 
----,KU.table_name as tablename, ku.table_schema
--FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS AS TC
--    JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS KU
--        ON TC.CONSTRAINT_TYPE = 'PRIMARY KEY' AND
--            TC.CONSTRAINT_NAME = KU.CONSTRAINT_NAME
--    left join sys.tables tbl 
--        on tbl.object_id = object_id(ku.table_schema + '.' + ku.table_name)
--    left join sys.columns c 
--        on tbl.object_id = c.object_id 
--            and ku.column_name = c.name
--    left join sys.types t
--        on c.system_type_id = t.system_type_id
--            
-- ORDER BY --KU.TABLE_NAME, 
--    KU.ORDINAL_POSITION

SELECT ku.table_schema as [schema], ku.table_name as [table], column_name, cast((case when tc.constraint_type is null then 0 else 1 end) as bit) isPK
                        --,object_id(ku.table_schema + '.' + ku.table_name) as tableObjId
                    ,t.name as TypeName
                FROM sys.tables tbl 
                
                left JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE ku 
                    on tbl.object_id = object_id(ku.table_schema + '.' + ku.table_name)
                
                left join INFORMATION_SCHEMA.TABLE_CONSTRAINTS tc
                    on TC.CONSTRAINT_TYPE = 'PRIMARY KEY' AND
                       TC.CONSTRAINT_NAME = KU.CONSTRAINT_NAME
                left join sys.columns c 
                    on tbl.object_id = c.object_id 
                        and ku.column_name = c.name
                left join sys.types t
                    on c.system_type_id = t.system_type_id
                order by ku.table_schema, ku.table_name, ku.ordinal_position
                
                
--sp_help 'accounts.account'