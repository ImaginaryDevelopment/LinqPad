<Query Kind="Statements" />

var top=100;
var sumOfSquares=Enumerable.Range(1,top).Select(i=>i*i).Sum();
var squareOfSum = Math.Pow(Enumerable.Range(1,top).Aggregate((x,y)=>x+y),2);

new{ sumOfSquares, squareOfSum, diff= squareOfSum-sumOfSquares}.Dump();
		