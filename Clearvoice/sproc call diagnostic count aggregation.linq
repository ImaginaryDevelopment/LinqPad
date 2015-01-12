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
		let sproc = line 
			.After("\"").Before("\"")
		let thread = line.After("Thread: ") 
		select new{ Sproc=sproc, thread,line}
		;

var qg = (from sproc in q
				group sproc by sproc into g
		select new{ g.Key.Sproc,Count= g.Count()})

	.OrderByDescending( sproc => sproc.Count)
////	.Select(a=>a.TrimStart().StartsWith("+\"")? a.After("\""):a)
////	.Dump()
////	.Select(a=>a.TrimStart().StartsWith("+")? a.After("+"):a)
////	.Select(a=>a.TrimStart().StartsWith("\"")? a.After("\""):a)
////	.Dump()
////	.Select(a=>a.TrimEnd().EndsWith("+")?a.BeforeLast("+"):a)
////	.Select(a=>a.TrimEnd().EndsWith("\"")?a.BeforeLast("\""):a)
//
//	//.Delimit(Environment.NewLine)
	.Dump();

var qg2 = q.GroupBy(g=> new{g.thread},g=> g.Sproc).Select(g=>new{Thread=g.Key,Count= g.Count(),
	Sprocs = g.OrderByDescending(g1=>g1)
		.Select(g2=>new{g2,Count=g2.Count()}).Distinct()}
		).Dump("by thread");
		
var qg3 =  q.GroupBy(g=> new{g.thread},g=> g.Sproc).Select(g=>new{Thread=g.Key,Count= g.Count(),
	Sprocs =q.Where(all=> all.thread==g.Key.thread).GroupBy(a1=>a1.Sproc,a1=>a1).Select(a2=>new{a2.Key,Calls=a2.Count()}).OrderByDescending(a=>a.Calls)}
		).Dump("by thread");
		
	input.Dump("input");