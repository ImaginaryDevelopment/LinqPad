<Query Kind="FSharpProgram" />

// phase 11 hours
[<Measure>] type Min // minutes
let replace (d) (r) = function | null|"" as x -> x | x -> x.Replace(oldValue=d,newValue=r)
module TimeSpan =
    let getTotalMinutes (ts:TimeSpan) = ts.TotalMinutes
    let toNearestMinutes minutes (ts:TimeSpan) =
        let totalMinutes = getTotalMinutes (ts + TimeSpan(0,minutes / 2<Min>, 0)) |> Convert.ToInt32
        TimeSpan(0,totalMinutes - totalMinutes % (minutes / 1<Min>), 0)
let validateOrder start finish =
    if start > finish then
        failwithf "bad validation"
let sort dt1 dt2 =
    if dt1 <= dt2 then dt1,dt2 else dt2,dt1
    
module Tuple2 =
    let map f (a,b)= f a, f b
    let rotate (a,b) = b,a
    let combine f (a,b) = f a b
    
type DateTimeSpan= {Started:DateTime;Finished:DateTime} with
    static member Combine (tolerance:float<Min>) (dts1:DateTimeSpan) (dts2:DateTimeSpan) =
        [dts1;dts2] |> Seq.iter (fun x -> validateOrder x.Started x.Finished)
        let tolerate (dt1:DateTime) (dt2:DateTime) = (dt2 - dt1) |> fun ts -> abs ts.TotalMinutes < (tolerance / 1.0<Min>)
        let x1,x2 = if dts1.Started < dts2.Started then dts1,dts2 elif dts2.Started < dts1.Started then dts2,dts1 elif dts1.Finished<= dts2.Finished then dts1,dts2 else dts2,dts1
        if x1.Started = x2.Started
            || tolerate x2.Started x1.Started
            || x1.Finished = x2.Finished
            || tolerate x2.Finished x1.Finished
            || tolerate x1.Finished x2.Started
            || x1.Started < x2.Started && x1.Finished > x2.Started
            then
            [{Started=x1.Started;Finished=max x1.Finished x2.Finished}]
        else [x1;x2]
           
            
        
module Spans =
    let dateTimeSpan dt1 dt2 =
        let dt1,dt2 = if dt1 < dt2 then dt1,dt2 else dt2,dt1
        validateOrder dt1 dt2
        {Started=dt1;Finished=dt2}
    let fold tolerance others dts2 : DateTimeSpan list =
        match others with
        | dts1::others ->
            (DateTimeSpan.Combine tolerance dts1 dts2|> List.rev)@others
                
        | [] -> [dts2]
    
module Testing =        
    type Tester = {Label:string;Dt1:DateTimeSpan;Dt2:DateTimeSpan;Tolerance:float<Min>;Expected:DateTimeSpan list}
    let tests =
        let now = DateTime.Now
        [
            (
                let start = now
                let finish = now.AddHours 2.0
                {Label="test tolerance on non-overlap";Dt1= Spans.dateTimeSpan start (now.AddMinutes 60.0); Dt2= Spans.dateTimeSpan (now.AddMinutes 64.0) finish;Tolerance=5.0<Min>;Expected=[{Started=start;Finished=finish}]}
            )
            (
                let start = now
                let finish = now.AddHours 2.0
                let dt1 =Spans.dateTimeSpan start (now.AddMinutes 60.0)
                let dt2 = Spans.dateTimeSpan (now.AddMinutes 64.0) finish
                // fold must reverse for head to be comparable to next for overlap properly
                {Label="test non-tolerant non-overlap";Dt1= dt1; Dt2= dt2;Tolerance=1.0<Min>;Expected=[dt2;dt1]}
            )
            ( 
                let start = now
                let finish = now.AddHours 2.0
                {Label="test actual overlap";Dt1= Spans.dateTimeSpan start (now.AddMinutes 60.0); Dt2= Spans.dateTimeSpan (now.AddMinutes 10.0) finish;Tolerance=5.0<Min>;Expected=[{Started=start;Finished=finish}]}
            )
        ]
        |> Seq.iter(fun x ->
            let actual = Spans.fold x.Tolerance [x.Dt1] x.Dt2 
            if actual <> x.Expected then
                (actual,x).Dump("fail")
                failwithf "bad fold"
        )
        
[
    
    "Oct 21 at 8:00 PM", "Oct 21 at 10:28 PM", "https://trello.com/c/y3UTePhe/3-by-10-24-year-over-year-data-on-data-entry-screen"
    "Oct 22 at 4:34 PM", "Oct 22 at 6:22 PM", "https://trello.com/c/y3UTePhe/3-by-10-24-year-over-year-data-on-data-entry-screen"
    "Oct 23 at 11:53 PM", "Oct 24 at 12:40 AM", "https://trello.com/c/y3UTePhe/3-by-10-24-year-over-year-data-on-data-entry-screen"
    "Oct 26 at 3:14 PM", "Oct 26 at 4:14 PM", "https://trello.com/c/y3UTePhe/3-by-10-24-year-over-year-data-on-data-entry-screen"
    "Oct 26 at 2:04 PM", "Oct 26 at 2:30 PM", "https://trello.com/c/OCJPaA7w/1-async-transmission"
    // override from working, removing hours
//    "Oct 30 at 2:15 PM", "Oct 30 at 9:10 PM", "https://trello.com/c/OCJPaA7w/1-async-transmission"
    "Oct 30 at 2:15 PM", "Oct 30 at 5:15 PM", "https://trello.com/c/OCJPaA7w/1-async-transmission"
    "Oct 25 at 9:03 AM", "Oct 25 at 5:36 PM", "https://trello.com/c/GyFSwZ0s/2-create-configurable-property-flag-on-the-event-object-to-show-another-set-of-input-boxes-for-all-of-the-biometric-entries-to-for"
    "Oct 30 at 1:35 PM", "Oct 30 at 4:26 PM", "https://trello.com/c/GyFSwZ0s/2-create-configurable-property-flag-on-the-event-object-to-show-another-set-of-input-boxes-for-all-of-the-biometric-entries-to-for"
    "Oct 26 at 2:30 PM", "Oct 26 at 3:14 PM", "https://trello.com/c/5UFr5kPa/5-install-as-a-service-the-server-app"
    
    // working card
    "Oct 21 at 8:00 PM", "Oct 21 at 10:28 PM", null
    "Oct 22 at 4:34 PM", "Oct 22 at 6:22 PM", null
    // bad tracking
//    "Oct 23 at 11:53 PM", "Oct 25 at 5:36 PM", null
    "Oct 26 at 2:04 PM", "Oct 26 at 4:14 PM", null
    "Oct 30 at 10:30 AM", "Oct 30 at 1:35 PM", null
    "Oct 30 at 1:35 PM", "Oct 30 at 5:15 PM", null 
    "Oct 30 at 8:16 PM", "Oct 30 at 9:10 PM", null
    
    
]
|> Seq.map(fun (st,sp,card) -> 
    let rep = replace "at " ", 2019 " >> DateTime.Parse
    rep st, rep sp, card
    ) // this will be wrong after 2019 on other phases
|> Seq.sortBy (fun (x,_,_) -> x)
//|> Dump

// implementation of card unfinished, would be nice to see what times do not have cards accounting for them
|> Seq.map(fun (start,fin,_card) -> Spans.dateTimeSpan start fin)
|> List.ofSeq
//|> Dump
|> List.fold (Spans.fold 5.0<Min>) []
|> List.rev
|> Dump
|> Seq.map(fun x -> x.Started,x.Finished,x.Finished - x.Started)
|> Seq.fold(fun (items,(rawTotal,roundedTotal)) (st,en,ts) ->
    let ts' = TimeSpan.toNearestMinutes 15<Min> ts
    (st,en,ts',ts)::items, (rawTotal + ts,roundedTotal + ts')
    
) (List.empty,(TimeSpan 0L, TimeSpan 0L))
|> fun (x,(t,rndt)) ->
    x,(sprintf "%i:%02i" (t.Days * 24 + t.Hours) t.Minutes, sprintf "%i:%02i" (rndt.Days * 24 + rndt.Hours) rndt.Minutes)
|> Dump
|> ignore