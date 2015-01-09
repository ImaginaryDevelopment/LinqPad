<Query Kind="FSharpProgram">
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll</Reference>
  <Reference>&lt;ProgramFilesX86&gt;\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll</Reference>
  <Reference>C:\projects\Fsi\tfsmacros.dll</Reference>
  <GACReference>Microsoft.TeamFoundation.WorkItemTracking.Client, Version=12.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a</GACReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Microsoft.TeamFoundation.Client</Namespace>
  <Namespace>Microsoft.TeamFoundation.VersionControl.Client</Namespace>
  <Namespace>Microsoft.TeamFoundation.WorkItemTracking.Client</Namespace>
  <Namespace>Macros</Namespace>
</Query>

let tfs = new TFS()
let workItemStore = tfs.Tfs.GetService<WorkItemStore>()
let iDictToDict<'a,'b when 'a:equality> (d:IDictionary<'a,'b>) = match d with :? System.Collections.IDictionary as x -> x | _ -> upcast Dictionary<'a,'b>(d)
	
let ActualHours = 10100;
let projectName = "Development";
let variables = dict ["project",projectName];
let workItemsAssignedToMe = 
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
				select (chwi.Id, chwi.Title, chwi.AreaId, chwi.AreaPath,chwi.IterationId, chwi.IterationPath, worked, 
					revisionDate, revisionDate.Value.DayOfWeek)
	}
	
display.Count().Dump("working revisions");			

let dic= dict [
				"today",DateTime.Today
				"last24Hours",DateTime.Today.AddDays( -1. )
				"lastWeek",DateTime.Now.StartOfWeek( DayOfWeek.Monday).AddDays( -7. )
				"last30Days",DateTime.Today.AddDays( -30. )
				]
	
let q= 
	query {
		for (id,title,areaId,areaPath,iterationId, iterationPath,worked,revisionDate,dayOfWeek) in display do
			for timeframe in dic.Keys do
				where (revisionDate.Value > dic.[timeframe])
				groupBy timeframe into g
				select g
		}
q.Dump();