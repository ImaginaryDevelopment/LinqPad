<Query Kind="Statements">
  <Connection>
    <ID>98063b47-89b9-46fb-a9bc-a976f3e1e9e6</ID>
    <Persist>true</Persist>
    <Server>wrdne21430,15001</Server>
    <Database>GTPM_Init1_SIT</Database>
    <DisplayName>GTPM.SIT</DisplayName>
  </Connection>
</Query>


var columns=@"	
LogFilename (S)             LogRow (I)                  date (T)
time (T)                    c-ip (S)                    cs-username (S)
s-sitename (S)              s-computername (S)          s-ip (S)
s-port (I)                  cs-method (S)               cs-uri-stem (S)
cs-uri-query (S)            sc-status (I)               sc-substatus (I)
sc-win32-status (I)         sc-bytes (I)                cs-bytes (I)
time-taken (I)              cs-version (S)              cs-host (S)
cs(User-Agent) (S)          cs(Cookie) (S)              cs(Referer) (S)
s-event (S)                 s-process-type (S)          s-user-time (R)
s-kernel-time (R)           s-page-faults (I)           s-total-procs (I)
s-active-procs (I)          s-stopped-procs (I)";

Func<string,string,string,string> delimit=(s1,s2,delimiter)=>s1+delimiter+s2;

Func<string,string,string> arrayDelimit=(s1,s2)=>delimit(s1,s2,"\",\"");
Func<string,string,string> enumDelimit=(s1,s2)=>delimit(s1,s2,","+Environment.NewLine);
var q= from c in columns.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
where string.IsNullOrWhiteSpace(c)==false
from f in c.Split(new[]{"  "},StringSplitOptions.RemoveEmptyEntries).Select(l=>l.Trim())
		select new{Column=f.Substring(0,f.IndexOf(" ")),Type=f.Substring(f.LastIndexOf("(")+1)} into a
		let typeLen=a.Type.Length
		
		select new {a.Column,Type=a.Type.Substring(0,typeLen-1)};
	
		var joined=q.Select(f=>f.Column).Aggregate(arrayDelimit);
		("new[]{\""+joined+"\"};").Dump("array");
		var enumFormatted=from g in q.OrderBy(f=>f.Type).GroupBy(f=>f.Type,(key,list)=>new{Key="/* "+key+" */",Values=list.Select(l=>l.Column)})
				select g.Key+Environment.NewLine+ g.Values.Select(e=>"///"+e+Environment.NewLine+e.Replace("-","_").Replace('(','_').Replace(")",null)).Aggregate(enumDelimit)+",";
		
		enumFormatted.Dump();
		
		
		//.Select(f=> f.Replace("-","_").Replace('(','_').Replace(")",null)+"/* "+f.Type+" */").Aggregate(enumDelimit).Dump("enum");
		q.Dump();