<Query Kind="Statements">
  <Connection>
    <ID>861d8a62-3a88-4251-9b53-d0afae78f70d</ID>
    <Persist>true</Persist>
    <Server>wrdne21430,15001</Server>
    <Database>GTPM_INIT1_DEV</Database>
    <DisplayName>GTPM.DEV</DisplayName>
    <IncludeSystemObjects>true</IncludeSystemObjects>
  </Connection>
</Query>

var location= System.IO.Directory.GetFiles(@"\\crprdnii1i4\sites\gtpm-init1\wwwroot\site");

location.Dump();
var webconfig=location.First(e=>e.EndsWith("Web.config"));
var attributes=System.IO.File.GetAttributes(webconfig);
attributes.Dump();
if((attributes & FileAttributes.ReadOnly)== FileAttributes.ReadOnly)
System.IO.File.SetAttributes(webconfig, attributes ^ FileAttributes.ReadOnly);