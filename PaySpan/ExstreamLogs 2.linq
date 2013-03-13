<Query Kind="Expression">
  <Connection>
    <ID>80dfcc92-5165-4166-86a3-559f60056be0</ID>
    <Persist>true</Persist>
    <Server>pqodb1be</Server>
    <Database>PaySpanHealth_Logging</Database>
  </Connection>
</Query>

Logs
	.Where (l =>new[]{"ws-printservices","HP Exstream Service"}.Contains( l.Logger))
	.OrderByDescending (x => x.Id)