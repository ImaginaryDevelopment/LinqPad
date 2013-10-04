<Query Kind="Program" />

void Main()
{
	var million= 1000*1000;
	million.ToString("C").Dump();
	var value=13L;
	var maxLength= Collatz(value).Dump().Count();
	new{value,maxLength}.Dump();
	for(var i=value+1L;i<=million;i++)
	{
		var len= Collatz(i).Count();
		if(len>maxLength){
			value=i;
			maxLength=len;
			
		}
	}
	new{value,maxLength}.Dump();
	Collatz(value).Dump("winner?");
}

// Define other methods and classes here

IEnumerable<long> Collatz(long seed){
	yield return seed;
	var value=seed;
	while(value>1){
		if(value %2==0){
			value=value/2;
			yield return value;
		} else {
			value=value*3+1;
			yield return value;
		}
	}
	//yield return 1;
}