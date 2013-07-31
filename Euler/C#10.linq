<Query Kind="Program">
  <Namespace>System.Threading.Tasks</Namespace>
</Query>

void Main()
{
var mil = 1000*1000;
int.MaxValue.ToString("C").Dump();
var top= 2*mil;

top.ToString("C").Dump();
var sw = new Stopwatch();
sw.Start();
	GetPrimes(2,10);
	var test=Primes.TakeWhile(a=>a<10).Aggregate((x,y)=> x+y);
	if(test!=17)
	{	
		test.Dump("failed!");
		return;
	}
	sw.Elapsed.Dump();
	GetPrimes(primeMax,top/4);
	Primes.TakeWhile(a=>a<top/4).Select(a=>(long)a).Aggregate((x,y)=> x+y).Dump();
	sw.Elapsed.Dump();
	sw.Restart();
	GetPrimes(primeMax,top/2);
	Primes.TakeWhile(a=>a<top/2).Select(a=>(long)a).Aggregate((x,y)=> x+y).Dump();
	
	sw.Elapsed.Dump();
	sw.Restart();
	GetPrimes(primeMax,top);
	var primes = Primes.TakeWhile(a=>a<top).ToArray();
	sw.Elapsed.Dump();
	sw.Restart();
	primes.Select(a=>(long)a).Aggregate((x,y)=> x+y).Dump();
	sw.Stop();
	sw.Elapsed.Dump();
	
}

static readonly HashSet<int> Primes= new HashSet<int>(){2};
static int nonPrimeMax=1;
static int primeMax=2;

void GetPrimes(int start,int stop){
	for(var i = Math.Max(2,start);i<stop;i++){
		if(IsPrime(i))
			Primes.Add(i);
		if(i%2!=0) //skip even numbers past 2
			i++;
	}
	
}

bool IsPrime(int val){
	if(val==2 || val<primeMax && Primes.Contains(val))
		return true;
	foreach(var p in Primes.AsParallel())
	{
		if(val % p ==0)
		{
			if(val>nonPrimeMax)
				nonPrimeMax=val;
		return false;
		}
	}
	var maxPrime= primeMax;
	var top= Math.Max(maxPrime+1,nonPrimeMax+1);
	if(top < val)
	foreach(var i in (top).To(val)){
		if(val % i ==0)
		{
			if(i>nonPrimeMax)
				nonPrimeMax=i;
			return false;
		}
	}
	
	if(val>primeMax)
		primeMax=val;
	Primes.Add(val);
	return true;
}