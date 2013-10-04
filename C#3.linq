<Query Kind="Program" />

void Main()
{
	PrimeFactorsOf(91).Dump();
	PrimeFactorsOf(13195).Dump();
	PrimeFactorsOf(600851475143).Dump();
}

static bool IsPrime(long value, IEnumerable<long> foundPrimes){
	
	foreach(var fp in foundPrimes){
		if(value % fp ==0)
		{
			//value.Dump("is not prime");
			return false;
		}
	}
	var highestPrime= foundPrimes.LastOrDefault();
	if(highestPrime==0)
	highestPrime=2;
	for(var i = highestPrime.Dump("searching for primes starting at");i<value;i++){ //search for next prime
		if(value % i ==0)
		return false;
	}
	return true;
}
// Define other methods and classes here
IEnumerable<long> PrimeFactorsOf(long input){
	var number = input;
	var index = 2L;
	var primes = new List<long>();
	while(index<=number){
		if(number % index ==0)
		{ //found a factor
		//check for prime and then reduce number
			if(IsPrime(index,primes)){
			primes.Add(index);
			yield return index;
			}
			number= number / index;
			number.Dump("reduced by factor:"+index);
		}
		if(index % 2L !=0) //skip even numbers after 2
			++index;
		index++;
	}
}