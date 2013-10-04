<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.Numerics.dll</Reference>
  <Namespace>System.Numerics</Namespace>
</Query>


var baseValue=2;
var pow=1000;
BigInteger value=2;
for(var i = 0; i<pow-1;i++){
	value=checked(value*baseValue);
	if(value %10==0)
	value = value /10;
}
value.ToString().ToCharArray().Select(a=>ulong.Parse(a.ToString())).Aggregate((a,b)=>a+b).Dump();