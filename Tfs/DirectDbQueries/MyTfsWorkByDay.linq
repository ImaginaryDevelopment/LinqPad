
void Main()
{
	// WorkItemChanges	.Where(wic=>wic.SystemId==3997)// wic.MicrosoftVSTSCommonResolvedBy=="Brandon D'Imperio")
	//WorkItemsLatestAndWere.Where(i=>i.ID==3997).OrderByDescending(a=>a.ChangedDate).Take(10).Dump();
	//this.Tbl_TeamConfigurationIterations.Dump();
	//this.Tbl_Iterations.Dump();
	var myPersonId= 4142;
	var qIteration = from wia in WorkItemsAres.Where(x=>x.AssignedTo==myPersonId && x.State!="Closed" && x.State!="Resolved")
					join iLeft in Tbl_Iterations on wia.IterationID equals iLeft.SequenceId into iL
					from iteration in iL.DefaultIfEmpty()
					select new {iteration.Iteration,wia};
	//qIteration.Dump();//.Select(x=>new{x.AreaID,x.IterationID, x.Title,x.WorkItemType}).Dump();
	var currentIterationId=330;
	
	
	var dic= new Dictionary<string,DateTime>{
		{"today",DateTime.Today},
		{"last24Hours",DateTime.Today.AddDays(-1)},
		{"lastWeek",DateTime.Now.StartOfWeek( DayOfWeek.Monday).AddDays(-7)},
		{"last30Days",DateTime.Today.AddDays(-30)}
	};
	foreach(var lbl in dic.Keys){
	
	//get my work items in in-progress or later status
	var myWorkItemProjections=GetMyWorkItemChangeProjections(currentIterationId,myPersonId,dic[lbl]).OrderByDescending(x=>x.GetDate().ToString("yyyyMMdd")).ThenBy(x=>x.Id);//.Dump();
	var totalHoursTracked=myWorkItemProjections.Sum(x=>x.Hours);
	var trackedWorkByDay = myWorkItemProjections.GroupBy(x=>x.Changed,x=>x.Hours);//.Dump();	
	var divisor=myWorkItemProjections.Select(x=>x.Changed).Distinct().Count();
	var avgTrackedHoursPerDay =totalHoursTracked/(divisor>0?divisor:1);
	new{ totalHoursTracked,avgTrackedHoursPerDay,trackedWorkByDay,myWorkItemProjections }.Dump(lbl,1);
	}
	//
}

public IEnumerable<Projection> GetMyWorkItemChangeProjections(int currentIterationId, int myPersonId,DateTime startDt){
var workItemIdsThisIteration=WorkItemsLatests.Where(i=>i.IterationID==currentIterationId).Select(i=>i.ID).Distinct().ToArray();//.Dump("workItemIds");
	//workItemIdsThisIteration=new[]{4004};
	
	var myWorkedWorkItems = WorkItemsLatests.Where(x=>x.AssignedTo==myPersonId && x.Fld10100>0 && workItemIdsThisIteration.Contains( x.ID)).OrderByDescending(x=>x.ID).ToArray();//.Dump("mine",0);
	
		Debug.Assert(myWorkedWorkItems.Count()==myWorkedWorkItems.Select(x=>x.ID).Distinct().Count());
	var myWorkItemIds= myWorkedWorkItems.Select(x=>x.ID);//.Dump("my workedWorkItemIds");
	Debug.Assert(myWorkItemIds.Count()==myWorkItemIds.Select(x=>x).Distinct().Count());
	
	//var days=Enumerable.Range(1,daysBackwards).Select(x=>DateTime.Today.AddDays(-x)).OrderByDescending(x=>x).ToArray();
	//var startDt=days.Min().Dump("startDt");
	//var endDt=days.Max().Dump("endDt");
	
		foreach(var myWorkItem in myWorkedWorkItems //.Where(x=>x==4004)
			){
			var allChanges=WorkItemsLatestAndWere.Where(x=>x.ID==myWorkItem.ID && x.Fld10100>0 && x.ChangedBy==myPersonId).DistinctBy(x=>x.Fld10100)
				.Select(x=>new{x.ID,x.IterationID, x.ChangedBy,x.ChangedDate,ActualHours=x.Fld10100})
				.OrderByDescending(x=>x.ChangedDate)
				.ToArray()
				.Concat(new[]{new{myWorkItem.ID,myWorkItem.IterationID, ChangedBy=(int?)myPersonId,ChangedDate=DateTime.MinValue,ActualHours=(int?)0}});
				//.Dump("allMyChanges ForWorkItem "+myWorkItem.ID);
				
				
			var deltas=allChanges.Zip(allChanges.Skip(1),(x,y)=>new{WorkItem=x,Previous=y}).Where(x=>x.WorkItem.ChangedDate>startDt);//.Dump("deltas");
			var projected= deltas.Select(x=>
				new Projection(x.WorkItem.ChangedDate){Id= x.WorkItem.ID,
					Hours= x.WorkItem.ActualHours.Value- x.Previous.ActualHours.GetValueOrDefault(0),
					HoursChangedTo=x.WorkItem.ActualHours.Value,
			HoursChangedFrom=x.Previous.ActualHours.GetValueOrDefault(0)});//.Dump("Projection for "+myWorkItem.ID);
			foreach(var p in projected)
				yield return p;
		}
		
}
public class Projection{
	readonly DateTime _changedDate;
	public Projection(DateTime changedDate){
		_changedDate=changedDate;
	}
	public int Id{get;set;}
	public string Changed{get{return _changedDate.ToShortDateString();}}
	
	public int Hours{get;set;}
	public int HoursChangedTo {get;set;}
	public int HoursChangedFrom {get;set;}
	
	public DateTime GetDate(){
		return _changedDate;
	}
}
// Define other methods and classes here