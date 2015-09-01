<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Numerics.dll</Reference>
</Query>

void Main()
{
	Debug.Assert(Routes(1)==2,"Failed 1");
	Debug.Assert(Routes(2)==6,"Failed 2");
	Debug.Assert(Routes(3)==20,"Failed 3");
	Routes(20).Dump();
	
	
}

System.Numerics.BigInteger Routes(uint size)
{
	return Combinations(size + size,size);
}
System.Numerics.BigInteger Combinations(uint n, uint r){
	var top = Factorial(n);
	var bottom = Factorial(n-r) * Factorial(r);
	return top/bottom;
}

System.Numerics.BigInteger Factorial(uint value){
	if(value== 0 || value ==1)
		return 1UL;
	 System.Numerics.BigInteger factorial = 1;
    for (int i = 1; i <= value; i++)
    {
        factorial *=(System.Numerics.BigInteger) i;
		//factorial.Dump();
    }
    return factorial;
	
}