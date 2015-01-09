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

let getAllFields = false

let tfs = new TFS()

let workItemStore = tfs.Tfs.GetService<WorkItemStore>()
let iDictToDict<'a,'b when 'a:equality> (d:IDictionary<'a,'b>) = match d with :? System.Collections.IDictionary as x -> x | _ -> upcast Dictionary<'a,'b>(d)
	
let ActualHours = 10100;
let CodeReviewer = 10106;
let projectName = "Development";
let variables = dict ["project",projectName];
let workItemsAssignedToMe justMe = 
	let queryText:string = 
		sprintf """select %s
			from WorkItems 
			where [System.State] <> 'Closed' and [System.State] <> 'Resolved'
			%s
			order by [System.State], [System.ChangedDate] desc""" fields
			<| ( if justMe then "and [System.AssignedTo] = @me " else String.Empty)
	
	workItemStore.Query(wiql=queryText) //,context = iDictToDict variables)
let items = workItemsAssignedToMe(true).Cast<WorkItem>()
items.Count().Dump("Work items")
items.Take(5).Dump();
if getAllFields then 
	items.Take(5).Dump();
else
	let q = query{
		for wi in items do
			yield wi.IterationPath, wi.Fields.["System.AssignedTo"], wi.Fields.GetType().GetMethods()
	}
	q.Take(5).Dump()