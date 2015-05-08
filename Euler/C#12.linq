<Query Kind="Program" />

void Main()
{
	var q= from n in GetNaturals().CumulativeSum()
			let f =GetFactors(n)
			select new{n,Factors=f.Count()};
			q.Take(10).Dump();
	q.Where(a=>a.Factors>500).Take(1).Dump();
}

// Define other methods and classes here
IEnumerable<int> GetNaturals(){
	foreach(var n in Enumerable.Range(1,int.MaxValue))
		yield return n;
}
public IEnumerable<int> GetFactors(int val){
	if( val==1)
	{
		yield return 1;
		yield break;
	}
	int sqrt = (int)Math.Ceiling(Math.Sqrt(val));
	 for (int i = 1; i < sqrt; i++)
    {
        if (val % i == 0)
        {
            yield return i;
			yield return val/i;
        }
    }
}