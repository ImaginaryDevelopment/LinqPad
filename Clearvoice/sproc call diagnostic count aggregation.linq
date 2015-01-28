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

var qg2 = q
	.GroupBy(g=> g.thread,g=>new{g.Sproc})
	.Select(g=>
		new{Thread=g.Key,ThreadCallCount= g.Count(),Sprocs=g.Select(g1=>new{g1.Sproc,Count= g.Count(i=>i.Sproc==g1.Sproc)}).Distinct().OrderByDescending(s=>s.Count)}
		).Dump("by thread");
	new Hyperlinq(()=> System.Windows.Forms.Clipboard.SetText(String.Join(Environment.NewLine, input)),"re-copy input").Dump();
	Util.OnDemand("input", ()=> input).Dump();