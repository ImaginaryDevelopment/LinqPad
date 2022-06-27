<Query Kind="FSharpProgram" />

// phase 12 hours
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
    "5/3/2021 8:40:49 AM","5/3/2021 9:44:11 AM","Add Participant Notes freetext field at end of questionnaire; add field to DB and store during event upload"
    "5/2/2021 12:25:26 PM","5/2/2021 2:32:53 PM","Add Participant Notes freetext field at end of questionnaire; add field to DB and store during event upload"
    "5/1/2021 7:14:10 PM","5/1/2021 8:19:33 PM","Add Participant Notes freetext field at end of questionnaire; add field to DB and store during event upload"
    "5/1/2021 6:37:45 PM","5/1/2021 6:43:14 PM","Add Participant Notes freetext field at end of questionnaire; add field to DB and store during event upload"
    "5/1/2021 6:43:12 PM","5/1/2021 7:11:43 PM","Add column on event selection page that shows \"Client\" [Name]"
    "5/1/2021 6:15:11 PM","5/1/2021 6:29:35 PM","Add column on event selection page that shows \"Client\" [Name]"
    "5/10/2021 9:23:07 PM","5/10/2021 10:49:05 PM","Change cache file save location to a folder that is not specific to the logged-in user."
    "5/1/2021 5:51:37 PM","5/1/2021 6:15:09 PM","Critical value popup font size too small"
    "5/3/2021 9:49:42 AM","5/3/2021 10:03:58 AM","Message to pass tablet back to participant is no longer showing up (bug)"
    "5/1/2021 5:41:52 PM","5/1/2021 5:51:26 PM","Message to pass tablet back to participant is no longer showing up (bug)"
    "5/1/2021 8:20:58 AM","5/1/2021 12:33:17 PM","Remove login window and instead \"log in\" the user with the Windows user account and pass that account name back to the server when logging"
    "4/30/2021 8:38:03 PM","4/30/2021 9:02:47 PM","Remove login window and instead \"log in\" the user with the Windows user account and pass that account name back to the server when logging"
    "4/29/2021 10:53:25 PM","4/30/2021 12:29:24 AM","Remove login window and instead \"log in\" the user with the Windows user account and pass that account name back to the server when logging"
    "4/27/2021 11:20:23 PM","4/27/2021 11:55:02 PM","Remove login window and instead \"log in\" the user with the Windows user account and pass that account name back to the server when logging"
    "5/2/2021 12:24:02 PM","5/2/2021 12:25:28 PM","Tapping \"Download event\" without an internet connection should show the user an error message: \"Please connect to Wifi to download event data.\""
    "5/1/2021 7:40:43 PM","5/1/2021 8:19:31 PM","Tapping \"Download event\" without an internet connection should show the user an error message: \"Please connect to Wifi to download event data.\""
    "5/1/2021 6:29:40 PM","5/1/2021 6:37:39 PM","Tapping \"Download event\" without an internet connection should show the user an error message: \"Please connect to Wifi to download event data.\""
    "5/11/2021 9:15:00 AM","5/11/2021 12:15:00 PM","Unbreaking the breaking change"
    "5/8/2021 12:13:09 AM","5/8/2021 12:13:53 AM","Unbreaking the breaking change"    
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