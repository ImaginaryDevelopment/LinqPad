<Query Kind="Program" />

void Main()
{
	var startHour=8;
	var startMinutes=45;
	var lunchMinutes=30;
	var includedLunch=DateTime.Now.Hour>11;
	var startTime=DateTime.Today.AddHours(startHour).AddMinutes(startMinutes);
	var grossWorked=(DateTime.Now - startTime);
	var currentHours=grossWorked.Add(includedLunch? TimeSpan.FromMinutes(-lunchMinutes):TimeSpan.FromMinutes(0)).Dump("current");
	var timeRemaining=TimeSpan.FromHours(8)- currentHours;
	timeRemaining.Dump("remaining" + (includedLunch? string.Empty: " skipping lunch"));
	
	Func<TimeSpan,TimeSpan> addALunch= lm=> includedLunch? timeRemaining: timeRemaining+lm;
	
	//+(includedLunch?TimeSpan.FromSeconds(0):TimeSpan.FromMinutes(60))
	//if(includedLunch)
	//DateTime.Now.Add(timeRemaining).Dump("stop");
	//else 
	new{ GrossRemaining=timeRemaining,
		Stop=DateTime.Now.Add(timeRemaining).ToString().After(" "),
		Remaining30=addALunch(TimeSpan.FromMinutes(30)),
		Stop30=DateTime.Now.Add( addALunch(TimeSpan.FromMinutes( 30))).ToTime(),
		Remaining60=addALunch(TimeSpan.FromMinutes(60)),
		Stop60=DateTime.Now.Add( addALunch(TimeSpan.FromMinutes( 60))).ToTime()
		}.Dump();
	var days= new[]{ 
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
		new TimeInput(new DateTime(2013,7,10,8,15,0),5,15,lunch:60)
		};
		
	var dq= from d in days
			let stop=d.End
			let worked=(stop-d.Start) -d.Lunch
			let workedMinutes=Math.Round(worked.Hours+ worked.Minutes/60.0m,2)
			select new{Date=d.Start.Date.ToString("yyyy-MM-dd"),Timesheet=workedMinutes, worked,Lunch=d.Lunch.TotalMinutes.ToString()+" Minutes",Stopped=stop.ToString("hh:mm:ss")};
			dq.OrderByDescending(d=>d.Date).Dump();

	var gross= 55.0m *(8-.15m);
	gross.Dump();
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