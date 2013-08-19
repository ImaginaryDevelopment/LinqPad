<Query Kind="Program" />

void Main()
{
	bool showMoney=false; //Util.ReadLine<bool>("show money?",false);
	var startHour=9;
	var startMinutes=0;
	var lunchMinutes=30;
	var targetHours=new TimeSpan(8,0,0).Dump("targetHours");
	var historyLimit= 8;
	var checkStartDate=new DateTime(2013,7,26);
	var checkEndDate= new DateTime(2013,8,10);
	var includedLunch=DateTime.Now.Hour>11;
	
	var startTime=DateTime.Today.AddHours(startHour).AddMinutes(startMinutes);
	var grossWorked=(DateTime.Now - startTime);
	var currentHours=grossWorked.Add(includedLunch? TimeSpan.FromMinutes(-lunchMinutes):TimeSpan.FromMinutes(0));
	var timeRemaining=TimeSpan.FromHours(8)- currentHours;
	
	//timeRemaining.Dump("remaining" + (includedLunch? string.Empty: " skipping lunch"));
	
	
	//ShowRemaining(includedLunch,timeRemaining);
	//+(includedLunch?TimeSpan.FromSeconds(0):TimeSpan.FromMinutes(60))
	
	var days= new[]{
	new TimeInput(new DateTime(2013,8,16,8,35,0),1,25,lunch:0),
	new TimeInput(new DateTime(2013,8,15,8,40,0),5,15,lunch:30),
	new TimeInput(new DateTime(2013,8,14,8,35,0),7,00,lunch:0),
	new TimeInput(new DateTime(2013,8,13,7,45,0),5,20,lunch:60),
	new TimeInput(new DateTime(2013,8,12,8,45,0),4,15,lunch:15),
	new TimeInput(new DateTime(2013,8,9,8,30,0),4,45,lunch:15),
	new TimeInput(new DateTime(2013,8,8,8,30,0),4,15,lunch:60),
	new TimeInput(new DateTime(2013,8,7,7,45,0),5,15,lunch:30),
	new TimeInput(new DateTime(2013,8,6,8,0,0),5,15,lunch:15),
	new TimeInput(new DateTime(2013,8,5,8,45,0),7,15,lunch:0),
	new TimeInput(new DateTime(2013,8,2,8,00,0),5,00,lunch:60),
	new TimeInput(new DateTime(2013,8,1,7,45,0),4,45,lunch:0),
	new TimeInput(new DateTime(2013,7,31,8,15,0),7,10,lunch:0),
		new TimeInput(new DateTime(2013,7,30,8,0,0),5,20,lunch:45),
		new TimeInput(new DateTime(2013,7,29,8,45,0),2,15,lunch:0),
		new TimeInput(new DateTime(2013,7,26,8,45,0),4,55,lunch:10),
		new TimeInput(new DateTime(2013,7,25,8,45,0),5,15,lunch:75),
		new TimeInput(new DateTime(2013,7,24,8,30,0),1,30,lunch:60),
		new TimeInput(new DateTime(2013,6,20,9,0,0),5,15,lunch:30),
		new TimeInput(new DateTime(2013,6,21,8,20,0),6,5,lunch:60),
		new TimeInput(new DateTime(2013,6,24,8,40,0),5,20,lunch:30),
		new TimeInput(new DateTime(2013,6,25,8,30,0),5,45,lunch:60),
		
		// 6/26 8:30 - 6:30 - 45 min over by 1 hr 15
		new TimeInput(new DateTime(2013,6,26,8,30,0),6,30,lunch:45),
		new TimeInput(new DateTime(2013,6,27,8,30,0),5,15,lunch:45),
		new TimeInput(new DateTime(2013,6,28,8,30,0),5,15,lunch:30),
		new TimeInput(new DateTime(2013,7,1,8,45,0),5,15,lunch:30),
		new TimeInput(new DateTime(2013,7,2,8,20,0),5,15,lunch:45),
		new TimeInput(new DateTime(2013,7,3,8,0,0),5,15,lunch:15),
		new TimeInput(new DateTime(2013,7,5,8,0,0),5,0,lunch:0),
		new TimeInput(new DateTime(2013,7,8,7,50,0),4,15,lunch:0),
		new TimeInput(new DateTime(2013,7,9,8,10,0),4,55,lunch:45),
		new TimeInput(new DateTime(2013,7,10,8,15,0),5,15,lunch:60),
		new TimeInput(new DateTime(2013,7,11,8,15,0),5,15,lunch:40),
		new TimeInput(new DateTime(2013,7,12,8,50,0),5,15,lunch:10),
		new TimeInput(new DateTime(2013,7,15,8,10,0),5,00,lunch:10),
		new TimeInput(new DateTime(2013,7,16,9,00,0),5,00,lunch:10),
		new TimeInput(new DateTime(2013,7,17,8,40,0),5,20,lunch:40),
		new TimeInput(new DateTime(2013,7,18,8,20,0),5,20,lunch:60),
		new TimeInput(new DateTime(2013,7,19,8,20,0),5,20,lunch:60),
		new TimeInput(new DateTime(2013,7,22,8,15,0),5,15,lunch:0),
		new TimeInput(new DateTime(2013,7,23,8,30,0),6,30,lunch:45),
		};
		var duplicates=days.Select(a=>a.Start.Date).Distinct().Count()!= days.Select(a=>a.Start.Date).Count();
		Debug.Assert(!duplicates,"Duplicate days detected");
		if(duplicates)
			return;
	//var estimatedNet= 55.0m *(8-.15m);
	var rate=decimal.Parse( Util.GetPassword("hourly"));
	var dq= from d in days
			let stop=d.End
			let worked=(stop-d.Start) -d.Lunch
			let workedMinutes=Math.Round(worked.Hours+ worked.Minutes/60.0m,2)
			let gross= rate*workedMinutes
			let net = gross * .85m
			select new{Date=d.Start.Date.ToString("yyyy-MM-dd"),Timesheet=workedMinutes,
				worked,
				Lunch=d.Lunch.TotalMinutes.ToString()+" Minutes",
				Stopped=stop.ToString("hh:mm:ss"),
				Net=showMoney? net:0.0m,
				Gross=showMoney? gross:0.0m, d};
				var currentMonday= DateTime.Now.Date.AddDays( (int)DateTime.Now.DayOfWeek * -1 +1);
				Debug.Assert(currentMonday.DayOfWeek== DayOfWeek.Monday);
	
	var hoursThisWk=dq.OrderByDescending(d=>d.Date).TakeWhile(a=>a.d.Start>=currentMonday).Select(a=>a.Timesheet);
	if(hoursThisWk.Any()==false){
		hoursThisWk=new[]{0m};
	}
	var totalHoursthisWk=hoursThisWk.Aggregate((a,b)=>a+b);
	var avgHoursThiswk= hoursThisWk.Average();
	new{
		Today=DateTime.Now.DayOfWeek.ToString(),
		Current=currentHours,
		Remaining8=timeRemaining,
		RemainingTarget=targetHours -currentHours ,
		includedLunch,
		HoursThisWeek=totalHoursthisWk,
		AvgHours=avgHoursThiswk ,
		Stop8=ShowRemaining(includedLunch,timeRemaining),
		StopTarget=targetHours.TotalMinutes!=8*60? ShowRemaining(includedLunch,targetHours-currentHours):null
		}.Dump();
	//ShowRemaining(includedLunch,timeRemaining);
	
	dq.Take(historyLimit).OrderByDescending(d=>d.Date).Dump();
	
	var paycheck = 
dq.Where(a=>string.Compare(a.Date,checkStartDate.ToString("yyyy-MM-dd"))>=0 
&& string.Compare(a.Date,checkEndDate.ToString("yyyy-MM-dd")) <=0).Dump("Paycheck");

	
	//gross.Dump();
}

public static object ShowRemaining(bool includedLunch, TimeSpan timeRemaining){
Func<TimeSpan,TimeSpan> addALunch= lm=> includedLunch? timeRemaining: timeRemaining+lm;
	if(includedLunch)
	return DateTime.Now.Add(timeRemaining).TimeOfDay.ToString().Before(".");
	else 
	return new{ GrossRemaining=timeRemaining,
		Stop=DateTime.Now.Add(timeRemaining).ToString().After(" "),
		Remaining30=addALunch(TimeSpan.FromMinutes(30)),
		Stop30=DateTime.Now.Add( addALunch(TimeSpan.FromMinutes( 30))).ToTime(),
		Remaining60=addALunch(TimeSpan.FromMinutes(60)),
		Stop60=DateTime.Now.Add( addALunch(TimeSpan.FromMinutes( 60))).ToTime()
		};
}
// Define other methods and classes here
public class TimeInput{
	public DateTime Start{get;private set;}
	public DateTime End{get;private set;}
	public TimeSpan Lunch{get;private set;}
	
	public TimeInput(DateTime start, int stopHour,int stopMinute, int lunch){
		Start=start;
		//assume PM (+12)
		End=new DateTime(Start.Year,Start.Month,Start.Day,stopHour+12,stopMinute,0);
		Lunch=TimeSpan.FromMinutes(lunch);
	}
}