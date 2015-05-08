<Query Kind="Program" />

void Main()
{
	//test the pyth algo
	IsPythagorean(3,4,5).Dump();
	var q = from a in Enumerable.Range(1,998).AsParallel()
			from b in Enumerable.Range(1,998).AsParallel()
			from c in Enumerable.Range(1,998).AsParallel()
			where Is1000Total(a,b,c) && IsPythagorean(a,b,c)
			select new{ a,b,c, Result = a*b*c};
			
	q.Dump();
}

// Define other methods and classes here
bool Is1000Total(int a, int b, int c){
	return a+b+c==1000;
}

bool IsPythagorean(int a, int b, int c){
	return Math.Pow(a,2)+Math.Pow(b,2)==Math.Pow(c,2);
}