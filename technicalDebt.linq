<Query Kind="Expression" />

from ms in MetricSummaries.Where( f=> f.Title=="PSAT.Web.MetricsCI")
join mf in MetricFiles on ms.MetricSummaryId equals mf.MetricSummaryId
join mt in MetricTypes on mf.MetricTypeId equals mt.MetricTypeId
where mf.ModuleName.StartsWith("Ajax")==false
where mf.ModuleName.StartsWith("DocumentFormat")==false
where mf.ModuleName.StartsWith("Glimpse")==false
where mf.ModuleName.StartsWith("Halos")==false
where mf.ModuleName.StartsWith("Ninject")==false
where mf.ModuleName.StartsWith("NuGet")==false
where mf.ModuleName.StartsWith("System")==false
where !(mf.MetricType.MetricShortName=="cc" && mf.Value<10)
where !(mf.MetricType.MetricShortName=="loc" && mf.Value<80)
orderby ms.Dt descending
group new {ms.Dt,mf.ModuleName,mt.MetricShortName, mf.Value,ms.MetricSummaryId, ms.SourcePath} 
 by new{mt.MetricShortName,mf.ModuleName}
 into groups

select new {groups.Key.ModuleName,groups.Key.MetricShortName, HighestValue=groups.Max(g=>g.Value),values=groups.Select(g=>new{ g.Dt,g.Value,g.MetricSummaryId})}