<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
</Query>

var q=
from cb in System.Windows.Forms.Clipboard.GetText().SplitLines()
	where cb.IsNullOrEmpty()==false && cb.Contains(".")
	let comment = cb.Contains("//")? " //"+cb.After("//"):string.Empty
	orderby cb
select new{Control=cb.Before("."),comment};

var output = "\t\tnew Control[] {"+Environment.NewLine+
	q.Select(a=> a.Control+","+a.comment).Delimit(Environment.NewLine)
	+ Environment.NewLine+"\t};";
	
	output.Dump();