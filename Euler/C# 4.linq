<Query Kind="Program" />

void Main()
{
var q = from i in Enumerable.Range(99,999-99)
		from j in Enumerable.Range(99,999-99)
		let result= i*j
		where IsPalindrome(result.ToString())
		select new{i,j,result};
		
q.OrderByDescending(a=>a.result).Take(1).Dump();
	
	
}

// Define other methods and classes here
bool IsPalindrome(string val){
	return val == new string(val.ToCharArray().Reverse().ToArray());
}