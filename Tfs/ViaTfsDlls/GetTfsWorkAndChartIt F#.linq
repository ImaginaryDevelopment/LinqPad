<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>C:\projects\Fsi\tfsmacros.dll</Reference>
  <GACReference>Microsoft.TeamFoundation.WorkItemTracking.Client, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>FSharp.Charting</NuGetReference>
  <Namespace>FSharp.Charting</Namespace>
  <Namespace>Macros</Namespace>
  <Namespace>Microsoft.TeamFoundation.Client</Namespace>
  <Namespace>Microsoft.TeamFoundation.VersionControl.Client</Namespace>
  <Namespace>Microsoft.TeamFoundation.WorkItemTracking.Client</Namespace>
  <Namespace>System.Windows.Forms</Namespace>
</Query>

// https://dotnetfiddle.net/UnS2vU
//printfn "starting up"
//
//let RedirectAssembly shortName (targetVersion : Version) publicKeyToken = 
//    let rec onResolveEvent = new ResolveEventHandler( fun sender evArgs ->
//        let requestedAssembly = 
//            AssemblyName(evArgs.Name)
//        if requestedAssembly.Name <> shortName 
//        then 
//            printfn "redirect firing for %s" requestedAssembly.Name; Unchecked.defaultof<Assembly>
//        else 
//            printfn 
//                "Redirecting assembly load of %s ,\tloaded by %s" 
//                evArgs.Name 
//                (if evArgs.RequestingAssembly = null then 
//                     "(unknown)"
//                 else 
//                     evArgs.RequestingAssembly.FullName)
//            requestedAssembly.Version <- targetVersion
//            requestedAssembly.SetPublicKeyToken (AssemblyName(sprintf "x, PublicKeyToken=%s" publicKeyToken).GetPublicKeyToken())
//            requestedAssembly.CultureInfo <- System.Globalization.CultureInfo.InvariantCulture
//            AppDomain.CurrentDomain.remove_AssemblyResolve(onResolveEvent)
//            Assembly.Load (requestedAssembly)
//            )
//    AppDomain.CurrentDomain.add_AssemblyResolve(onResolveEvent)
//    
//RedirectAssembly "FSharp.Core" (Version("4.3.1.0")) "b03f5f7f11d50a3a"

type FSharp.Charting.ChartTypes.GenericChart with 
    member this.CreateForm ()= 
        let frm = new Form(Visible = true, TopMost = true, Width = 700, Height = 500, WindowState=FormWindowState.Maximized)
        let ctl = new FSharp.Charting.ChartTypes.ChartControl(this, Dock = DockStyle.Fill)
        frm.Controls.Add(ctl)
        frm.Show()
        ctl.Focus() |> ignore
        frm

let iDictToDict<'a,'b when 'a:equality> (d:IDictionary<'a,'b>) = match d with :? System.Collections.IDictionary as x -> x | _ -> upcast Dictionary<'a,'b>(d)


let tfs = new TFS()
let workItemStore = tfs.Tfs.GetService<WorkItemStore>()

	
let ActualHours = 10100;
let projectName = "Development";
let variables = dict ["project",projectName];
let workItemsAssignedToMe = 
	let queryText = "select [System.Id], [System.AreaPath], [System.Title], [System.State], [OceansideTen.ReportedBy] from WorkItems where [System.TeamProject] = @project and [System.State] <> 'Closed' and [System.State] <> 'Resolved' and [System.AssignedTo] = @me order by [System.State], [System.ChangedDate] desc"
	workItemStore.Query(wiql=queryText,context = iDictToDict variables)
workItemsAssignedToMe.Count.Dump("Work items")
let changedHours = 
	workItemsAssignedToMe
		.Cast<WorkItem>()
		.Where(fun wi->
			wi.Revisions
				.Cast<Revision>()
				.Any(fun r->r.Fields.TryGetById(ActualHours)<>null && r.Fields.TryGetById(ActualHours).IsChangedInRevision))
		//.Dump()
type WorkHistory = {WorkItemId:int;Title:string;AreaId:int;AreaPath:string;IterationId:int;IterationPath:string; Worked:int; RevisionDate: DateTime}
let display = 
	query {
		for chwi in changedHours do
			for revision in chwi.Revisions.Cast<Revision>() do
				let actualHoursField = revision.Fields.TryGetById(ActualHours)
				where (actualHoursField <> null && actualHoursField.Value <> null)
				let revisionDateField = revision.Fields.TryGetById( -5 )
				where (revisionDateField  <> null && actualHoursField.Value <> null)
				let revisionDate =  
					match revisionDateField.Value with 
					| :? DateTime as dt -> Some dt 
					| _ -> None
				where (revisionDate.IsSome)
				
				let actualHours = actualHoursField.Value :?> int
				let originalHours = 
					match actualHoursField.OriginalValue with 
					| :? int as x -> x | _ -> 0
				let worked = actualHours - originalHours
				where (worked > 0 && revisionDate.Value < new DateTime(2100,1,1)) // some dates were 1/1/9999
				sortByDescending revisionDate
				select {
					WorkHistory.WorkItemId = chwi.Id
					Title = chwi.Title
					AreaId = chwi.AreaId
					AreaPath = chwi.AreaPath
					IterationId = chwi.IterationId
					IterationPath = chwi.IterationPath
					Worked = worked
					RevisionDate = revisionDate.Value
					}
	}

display.Count().Dump("working revisions");			

let startOflast2Weeks = DateTime.Now.StartOfWeek( DayOfWeek.Monday).AddDays( -7. )
startOflast2Weeks.Dump("start!")
let xFormat (dt:DateTime) = dt.DayOfWeek.ToString() + "," + dt.ToShortDateString()
let onlyLast2Weeks = 
	display 
	|> Seq.filter( fun wh -> wh.RevisionDate > startOflast2Weeks)
	|> Seq.sortBy ( fun wh -> wh.RevisionDate)
	
let byDay = 
	onlyLast2Weeks 
	|> (fun wh -> ([for workHistory in wh -> xFormat workHistory.RevisionDate.Date, workHistory.Worked]))
let byWeek = 
	let startDt = startOflast2Weeks
	let endDt = startOflast2Weeks.AddDays(7.)
	let sumForDt (dt:DateTime) = onlyLast2Weeks |> Seq.filter(fun wh -> wh.RevisionDate > dt.StartOfWeek(DayOfWeek.Monday) && wh.RevisionDate < dt.StartOfWeek(DayOfWeek.Monday).AddDays(7.)) |> Seq.map (fun wh -> wh.Worked) |> Seq.sum
	seq {
		for wh in onlyLast2Weeks do
			yield xFormat wh.RevisionDate,sumForDt 	wh.RevisionDate
	}
byWeek.Dump("byWeek")	
let workChart = 
	Chart.Combine (
		[
			Chart.Line(byDay,Name="byDay",Title="WorkedByDay").WithXAxis(Enabled = true)
			Chart.Line(byWeek,Name="byWeek",Title="WorkedByWeek").WithXAxis(Enabled = true) //.WithXAxis(Min = 0., Max = 1.)
		])
	|> Chart.WithLegend()
let chartForm = workChart.CreateForm()
System.Windows.Forms.Application.Run(chartForm)
"finished!".Dump()