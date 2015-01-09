<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Namespace>System.Web</Namespace>
</Query>

//C# to sql
//TODO: find a way to group/compartmentalize (or break out) by section of code or order of operations(etc..)
var input=System.Windows.Forms.Clipboard.GetText().SplitLines();
var q= from line in input
		where line.Contains("Reader") && line.Contains("].[")
		
		select line 
			.After("\"").Before("\"")
		;
var qg = (from sproc in q
		group sproc by sproc into g
		select new{ g.Key,Count= g.Count()})
	.OrderByDescending( sproc => sproc.Count)
//	.Select(a=>a.TrimStart().StartsWith("+\"")? a.After("\""):a)
//	.Dump()
//	.Select(a=>a.TrimStart().StartsWith("+")? a.After("+"):a)
//	.Select(a=>a.TrimStart().StartsWith("\"")? a.After("\""):a)
//	.Dump()
//	.Select(a=>a.TrimEnd().EndsWith("+")?a.BeforeLast("+"):a)
//	.Select(a=>a.TrimEnd().EndsWith("\"")?a.BeforeLast("\""):a)

	//.Delimit(Environment.NewLine)
	.Dump();