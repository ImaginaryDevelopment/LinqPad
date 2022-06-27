<Query Kind="SQL">
  <Connection>
    <ID>6d9036c3-dd08-4e55-8c56-0d7d31c5ebfc</ID>
    <Persist>true</Persist>
    <Server>prog7-pc</Server>
    <SqlSecurity>true</SqlSecurity>
    <Database>ApplicationDatabase</Database>
    <UserName>xpu10</UserName>
  </Connection>
</Query>

--create table TriggerTest(
--	ID int identity primary key,
--	Created datetime not null default getutcdate()
--)
--alter trigger trig_testTrigger on patientsinfo after UPDATE as begin
-- insert into triggerTest (created) values ('2018-02-28')
--end
--ALTER TABLE patientsinfo DISABLE TRIGGER trig_testtrigger
--insert into triggerTest default values
--select * from triggerTest
--alter table patientsinfo enable trigger trig_testtrigger

-- http://geekswithblogs.net/KingSurfers_Brain/archive/2008/05/30/find-all-triggers-and-their-text-with-t-sql.aspx
SELECT      Tables.Name TableName,
      Triggers.name TriggerName,
      Triggers.crdate TriggerCreatedDate,
      Comments.Text TriggerText
FROM      sysobjects Triggers
      Inner Join sysobjects Tables On Triggers.parent_obj = Tables.id
      Inner Join syscomments Comments On Triggers.id = Comments.id
WHERE      Triggers.xtype = 'TR'
      And Tables.xtype = 'U'
ORDER BY Tables.Name, Triggers.name
