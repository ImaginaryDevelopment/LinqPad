<Query Kind="Program" />

void Main()
{
const int bottom=1;
	var top=20;
	for (int i = top; i < int.MaxValue; i++)
	{
		if(IsDivisibleByAll(1,top,i)){
			new{ bottom,top,i}.Dump();
			return;
		}
	}
}

// Define other methods and classes here
bool IsDivisibleByAll(int start, int end,int value){
	for (int i = start; i <= end; i++)
	{
		if( value % i !=0)
		return false;
	}
	return true;
}