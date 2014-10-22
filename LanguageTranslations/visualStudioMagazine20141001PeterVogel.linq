<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>System.Reactive.Linq</Namespace>
  <Namespace>System.Reactive.Subjects</Namespace>
  <Namespace>System.Windows.Forms</Namespace>
</Query>

void Main()
{
	ISubject<StatusChange> statChange= new Subject<StatusChange>();
	statChange.Subscribe(sc => MessageBox.Show(sc.OrderStatus));
	
	statChange.OnNext(new StatusChange(){ OrderId=1, OrderStatus = "New"});
	var order = new SalesOrder();
	statChange.Subscribe(StatusChanged);
	statChange.Subscribe(StatusAudit);
	statChange.Where(c => c.OrderStatus=="Processing").Subscribe(ReportStatusChange);
	var scs = from sc in statChange
			where sc.OrderStatus =="Processing"
			select sc;
	scs.Subscribe(ReportStatusChange);
	var sub = (from sc in statChange
				where sc.OrderStatus =="Processing"
				select sc).Subscribe(ReportStatusChange);
	statChange.Subscribe(OnNext,OnError,OnCompleted);
	
	order.StatChange.Subscribe(new StatusMonitor());
	
	statChange.OnError(new Exception("Something has gone horribly wrong!"));
	
}

public static void OnNext(StatusChange status){
	ReportStatusChange(status);
}

public static void OnError(Exception ex){
	Console.Error.WriteLine(ex.Message);
}
public static void OnCompleted(){
	Console.WriteLine("order processing completed");
}

public static void ReportStatusChange(StatusChange status){
	Console.WriteLine("Reporting status change {0}", status.OrderStatus);
}
public static void StatusChanged(StatusChange status){
	Console.WriteLine(status.OrderStatus);
}
public static void StatusAudit(StatusChange status){
	Debug.WriteLine("status changed to {0}",status);
}
// Define other methods and classes here
public class StatusChange
{
	public int OrderId {get;set;}
	public string OrderStatus {get;set;}
}

public class SalesOrder
{
	string _status;
	public ISubject<StatusChange> StatChange{get;private set;}
	public int Id{get;set;}	
	public string Status{
		get{
			return _status;
		}
		set{
			_status = value;
			var sc = new StatusChange() { OrderId = this.Id, OrderStatus = this.Status};
			StatChange.OnNext(sc);
		}
	}
	public SalesOrder(){
		StatChange = new Subject<StatusChange>();
	}
	
}

public class StatusMonitor : IObserver<StatusChange>
{
	public void OnNext(StatusChange value){
		MessageBox.Show(value.OrderStatus);
	}
	
	public void OnError(Exception ex){
		MessageBox.Show(ex.Message);
	}
	public void OnCompleted(){
		MessageBox.Show("Done");
	}
}
	