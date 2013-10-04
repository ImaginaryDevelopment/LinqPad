<Query Kind="Program" />

void Main()
{
	GetAmicables(10000).Dump("Amicables").Sum().Dump("total");
	
	GetFactors(220).Sum().Dump();
	GetFactors(284).Sum().Dump();
}
IEnumerable<int> GetAmicables(int limit){
var dic= new Dictionary<int,int>(); //key + sum + factors
	for(var i = 4; i<limit; i++){
		var sum=GetFactors(i).Sum();
		dic[i]=sum;
		if(dic.ContainsKey(sum) && dic[sum]==i  && sum!=i)
		{
			yield return i;
			yield return sum;
		}
		
	}

}
// Define other methods and classes here
IEnumerable<int> GetFactors(int value){
	if(value>1)
	yield return 1;
	var bound = (int) Math.Sqrt(value);
	for(var i=2;i<bound;i++){
		if(value % i==0){
			yield return i;
			yield return value / i;
		}
	}
}