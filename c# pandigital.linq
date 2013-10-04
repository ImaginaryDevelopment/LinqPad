<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Numerics.dll</Reference>
  <Namespace>System.Numerics</Namespace>
</Query>

void Main()
{
Enumerable.Range(1,9).Dump();
	//var count = 3;
	var start = 1;
	//var x = 192;
	//Pandigital(start,count,x).Dump();
	//Pandigital(1,5,9).Dump();
	for(var i=2; i<450;i++){
		var p = Pandigital(1,9,i);
	}
	var q= from i in Enumerable.Range(1,40)
			//from cnt in Enumerable.Range(2,8)
			let p = Pandigital(start,9,i)
			//where p.Length==9
			let pv = BigInteger.Parse(p)
			where pv >= 192384576ul
			orderby pv descending
			select new{i, // cnt,
			p, len= p.Length};
			
			q.Dump();
	
	
	
}

// Define other methods and classes here
public string Pandigital(int start, int count, int x){
var concat = string.Empty;
	foreach(var i in Enumerable.Range(start,count))
		concat+= (((BigInteger)x)*((BigInteger)i)).ToString();
		
return concat;
}