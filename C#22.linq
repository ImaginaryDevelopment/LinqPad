<Query Kind="Statements" />

var q=from l in System.IO.File.ReadAllText(@"D:\Users\DBee\Downloads\names.txt").Split(new[]{','}).Select(n=>n.Trim(new[]{'"'}))
orderby l
select new{l,letters=l.ToCharArray().Select(a=>(int)a-64), total= l.ToCharArray().Select(a=>(int)a-64).Sum()};
var scoreQ=q.Select((n,i)=>new{Name=n.l, n.letters, n.total,i, Score= n.total*(i+1)});
scoreQ.Where(n=>n.Name=="COLIN").Dump();

scoreQ.Select(a=>a.Score).Sum().Dump();

