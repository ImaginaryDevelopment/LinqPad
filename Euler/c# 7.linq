<Query Kind="Program" />

void Main()
{
	GetPrimes(2).Skip(10000).Take(1).Dump();
}

static readonly List<int> Primes= new List<int>(){2};


IEnumerable<int> GetPrimes(int start){
	for(var i = Math.Max(2,start);i<int.MaxValue;i++){
		if(IsPrime(i))
			yield return i;
		if(i%2!=0) //skip even numbers past 2
			i++;
	}
	yield break;
}

bool IsPrime(int val){
	if(Primes.Contains(val))
		return true;
	foreach(var p in Primes)
	{
		if(val % p ==0)
		return false;
	}
	for(var i =Primes.Max()+1;i<val;i++){
		if(val % i ==0)
			return false;
	}
	return true;
}