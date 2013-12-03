<Query Kind="Program" />

void Main()
{
	var q = from l in text.SplitLines()
		let rewardType=Regex.Match(l,@"^(\w+\s?){1,3}(?= for)").Value
		let description = Regex.Match(l.After("for"),@"(\w+\s?){1,3}(?= occurred)").Value
		let dtVal=l.After("occurred on").Before("for")
		let dt=DateTime.Parse( dtVal)
		let amountVal=l.After(dtVal).After(" for $")
		let amount=decimal.Parse(amountVal.BeforeOrSelf("Study").TrimEnd().TrimEnd('.')	)
		let rewardId=amountVal.Contains("Study")?amountVal.After("Study #"):string.Empty
		select new{rewardType,description,dt,amount,rewardId,l};
	q.Dump();
	
	q.Select(a=>string.Format(
		"new Rewarded{{ RewardType=\"{0}\", Description=\"{1}\", RewardDt= new DateTime({2},{3},{4},{5},{6},{7}), Amount= {8}m, RewardId=\"{9}\"}},",
			a.rewardType,a.description,a.dt.Year,a.dt.Month,a.dt.Day,a.dt.Hour,a.dt.Minute,a.dt.Second,a.amount,a.rewardId)).Dump();
}

// Define other methods and classes here
string text=@"Completed survey reward for Beverage Survey occurred on 2/23/2010 10:46:52 AM for $0.50.	Study #28068
Terminated survey for Restaurant Survey occurred on 2/3/2010 12:48:34 PM for $0.10.	Study #27108
Terminated survey for Clinical Trial Invitation occurred on 2/1/2010 3:43:20 PM for $0.10.	Study #26778
Terminated survey for Auto Consumer Survey occurred on 1/18/2010 2:52:55 PM for $0.10.	Study #25019
Terminated survey for Business Accounting Survey occurred on 12/15/2009 1:25:40 PM for $0.10.";