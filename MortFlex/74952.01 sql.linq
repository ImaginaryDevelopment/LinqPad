<Query Kind="SQL">
  <Connection>
    <ID>4c34d230-33fe-4a9c-ba77-3dc37aa14f5b</ID>
    <Persist>true</Persist>
    <Server>rpsql2008r2dev\dev</Server>
    <SqlSecurity>true</SqlSecurity>
    <Database>STD08RLSD</Database>
    <UserName>WINRLS</UserName>
    <Password>AQAAANCMnd8BFdERjHoAwE/Cl+sBAAAA1WLKs9qc4USFiwcJ5tmkhgAAAAACAAAAAAADZgAAwAAAABAAAACrzDqjgelbVgZelHzxoUCkAAAAAASAAACgAAAAEAAAANaxhqC+PlSpfWS3MUIfULoIAAAAIqZ2P2kltAwUAAAAsZVmuOxGnoVr0wOGD/We2Jqm8Z4=</Password>
  </Connection>
</Query>

alter table mitem add MESSAGE_CODE NVARCHAR(4)
alter table parm add DU_FINDING_TYPE
--insert into GENCODE2(groups, code, description) values(