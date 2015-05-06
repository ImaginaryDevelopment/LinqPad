<Query Kind="SQL">
  <Connection>
    <ID>4e94eacc-a31d-4687-947b-e4c9804c895a</ID>
    <Persist>true</Persist>
    <Server>(local)</Server>
    <IncludeSystemObjects>true</IncludeSystemObjects>
    <Database>XPEncounter</Database>
    <ShowServer>true</ShowServer>
  </Connection>
</Query>

--DCFollowUps.Select(d=> d.AuthorXML.ToString().ToCharArray()).Take (100)

--select datalength(authorxml) from dcfollowups
select authorxml from [dbo].[Encounters] for xml auto
select case 
	when authorxml is null then 'null' 
	when datalength(authorxml) is not null and datalength(authorxml) > 5 then convert(varchar,datalength(authorxml))
	when convert(varchar, authorxml) = '' then 'empty' 
	when convert(varchar, authorxml) = ' ' then 'whitespace' 
	else convert(varchar(max), authorxml) end
from dbo.Encounters

update dbo.Encounters
set authorxml = null
where convert(varchar(max), authorxml) = ''

--outer apply ax.XmlData.authorxml('.')