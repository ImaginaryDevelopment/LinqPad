<Query Kind="FSharpProgram">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <NuGetReference>Rx-Main</NuGetReference>
  <Namespace>System.Reactive</Namespace>
</Query>

let smallSample = """<TextBlock Grid.Column="0" Text="{Binding PayerID, Converter={StaticResource PayerIDToGuarantorProfileConverter}}" />
                <TextBlock Grid.Column="1" Text="{Binding Charge, StringFormat={}{0:C}}" />
                <TextBlock Grid.Column="2" Text="{Binding Allowed, StringFormat={}{0:C}}" />"""
let dir = "C:/TFS"
let getDirs p = System.IO.Directory.GetDirectories p
let getFiles p = System.IO.Directory.GetFiles p
type TryGetTextResult = 
	| Success of string
	| Failure of Exception
let tryGetText filepath = 
	try
		System.IO.File.ReadAllText(filepath) |> TryGetTextResult.Success
	with
		| :? IOException as iex -> TryGetTextResult.Failure iex
type ObservableSource<'t>() = // Control.Observable Module - https://msdn.microsoft.com/en-us/library/ee370313.aspx
	let protect function1 = 
		let mutable ok = false
		try
			function1()
			ok <- true
		finally
			Debug.Assert(ok, "IObserver method threw an exception.")
	let mutable key = 0
	let mutable subscriptions = Map.empty: Map<int,IObserver<'t>>
	let withValue f = 
		subscriptions |> Seq.iter (fun (KeyValue(_,value)) ->
			protect (fun () -> f value))
//	let next(obs) = 
//		subscriptions |> Seq.iter (fun (KeyValue(_, value)) ->
//			protect (fun () -> value.OnNext(obs)))
	let next(obs) =   withValue (fun v -> v.OnNext(obs))
	let completed() = withValue (fun v -> v.OnCompleted())
	let error(err) =  withValue (fun v -> v.OnError(err))
	let thisLock = new obj()
	let obs = 
		{new IObservable<'t> with
			member this.Subscribe(obs) =
				let key1 = 
					lock thisLock (fun () ->
						let key1 = key
						key <- key + 1
						subscriptions <- subscriptions.Add(key1, obs)
						key1)
				{ new IDisposable with
					member this.Dispose() = 
						lock thisLock (fun () -> subscriptions <- subscriptions.Remove(key1))
				}
		}
	let mutable finished = false
	let assertUnfinished() = 
		Debug.Assert(not finished, "IObserver is already finished")
	member this.Next(obs) =
		assertUnfinished()
		next obs
	member this.Completed() =
		assertUnfinished()
		finished <- true
		completed()
	member this.Error(err) = 
		assertUnfinished()
		finished <- true
		error err
	member this.AsObservable = obs
			
type FindDupesResult =
	|FileReadFailure of Exception
	|DirReadFailure of Exception
	|DupeFound
let createIncObservable ()=
	let source = new ObservableSource<int>()
	let mutable value = 0
	let increment () = 
		value <- value + 1
		source.Next(value)
	let completed () = source.Completed()
	increment,source.AsObservable,completed
let createIncLive desc =
	let inc,src,completed =createIncObservable()
	src.DumpLatest(desc,true) |> ignore
	inc,completed

let directoryChecked,directoriesCompleted =createIncLive "directoriesChecked"

let fileChecked, filesCompleted = createIncLive "filesChecked"

let rec findDupes code dir =
	directoryChecked()
	seq {
		for f in getFiles dir do
			let text = tryGetText f
			match text with
			|Success text -> 
				fileChecked()
				if text.Contains(code) then yield (DupeFound, f)
			|Failure ex -> yield (FileReadFailure ex, f)
		for d in getDirs dir do
			yield! findDupes code d
	}
	
let findAsync () = 
	let r = async {
		return findDupes smallSample dir
		
		}
	r.GetType().Dump()
	
	r.Dump()
	directoriesCompleted()
	filesCompleted()