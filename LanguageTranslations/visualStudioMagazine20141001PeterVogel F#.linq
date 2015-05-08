<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>System.Reactive.Linq</Namespace>
  <Namespace>System.Reactive.Subjects</Namespace>
  <Namespace>System.Windows.Forms</Namespace>
</Query>

type StatusChange = {OrderId:int; OrderStatus:string}
type SalesOrder() =
	let mutable _status = String.Empty
	let statChange = new Subject<StatusChange>()
	member this.StatChange:ISubject<StatusChange> = upcast statChange
	member val Id = 0 with get,set
	member this.Status
		with get() = _status
		and set(value) = 
			_status <- value
			this.StatChange.OnNext {OrderId = this.Id; OrderStatus = this.Status}
	
type StatusMonitor() =
	interface IObserver<StatusChange> with 
		member this.OnNext value = MessageBox.Show(value.OrderStatus) |> ignore
		member this.OnError ex = MessageBox.Show(ex.Message) |> ignore
		member this.OnCompleted() = MessageBox.Show("Done") |> ignore
	
let ReportStatusChange status = printfn "Reporting status change %s" status.OrderStatus
let OnCompleted() = Console.WriteLine("order processing completed")
let OnError (ex:Exception) = Console.Error.WriteLine(ex.Message)
let OnNext status = ReportStatusChange status
let StatusChanged status = Console.WriteLine(status.OrderStatus);
let StatusAudit status = Debug.WriteLine("status changed to {0}",[status])

let statChange= new Subject<StatusChange>();
statChange.Subscribe( fun sc -> MessageBox.Show(sc.OrderStatus) |> ignore) |> ignore

statChange.OnNext({ OrderId=1; OrderStatus = "New"})
let order = new SalesOrder()
statChange.Subscribe(StatusChanged) |> ignore
statChange.Subscribe(StatusAudit) |> ignore
statChange.Where( fun c -> c.OrderStatus="Processing").Subscribe(ReportStatusChange) |> ignore

statChange.Subscribe(OnNext,OnError,OnCompleted);

order.StatChange.Subscribe(new StatusMonitor());


(* work in progress 
let scs = 
	query {
		for sc in statChange do
		where (sc.OrderStatus = "Processing")
		select sc
	}
scs.Subscribe(ReportStatusChange);
*)
//let sub = 
//	(query{
//			for sc in statChange do
//			where (sc.OrderStatus ="Processing")
//			select sc}).Subscribe(ReportStatusChange)

let sub = statChange.Where( fun c -> c.OrderStatus="Processing").Subscribe(ReportStatusChange)
statChange.OnError(new Exception("Something has gone horribly wrong!"));



	