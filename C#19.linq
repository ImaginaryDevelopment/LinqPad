<Query Kind="Statements" />

var start=new DateTime(1900,1,1);
var end=new DateTime(2000,12,31);
var daysTotal=(start - end);

var sundays=0;
for(var y =1901;y<=2000;y++)
for(var m =1;m<=12; m++){
	var dt=new DateTime(y,m,1);
	if(dt.DayOfWeek== DayOfWeek.Sunday){
		//new{dt,day=dt.DayOfWeek}.Dump();
		sundays++;
	}
}
sundays.Dump();