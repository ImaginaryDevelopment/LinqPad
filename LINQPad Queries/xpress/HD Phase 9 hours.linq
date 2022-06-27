<Query Kind="FSharpProgram" />

// phase 9 hours
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
//    "Jan 22 at 4:33 PM", "Jan 22 at 6:36 PM"
    "Jun 13 at 5:57 PM", "Jun 13 at 8:45 PM" // https://trello.com/c/4jvlSXwR/7-change-user-password-hashing-algorithm-to-sha256-and-use-prefix-and-suffix-salt
    "Jun 14 at 10:15 AM", "Jun 14 at 2:10 PM" // https://trello.com/c/4jvlSXwR/7-change-user-password-hashing-algorithm-to-sha256-and-use-prefix-and-suffix-salt
    "May 23 at 5:40 PM", "May 23 at 7:47 PM" // https://trello.com/c/BhAdNNtf/2-add-signature-drawing-on-consent-form-clickthrough-collect-signature-and-record-timestamp-of-clicking-agree
    "May 28 at 4:40 PM", "May 28 at 7:48 PM" // https://trello.com/c/BhAdNNtf/2-add-signature-drawing-on-consent-form-clickthrough-collect-signature-and-record-timestamp-of-clicking-agree
    "May 30 at 5:56 PM", "May 30 at 8:12 PM" // https://trello.com/c/BhAdNNtf/2-add-signature-drawing-on-consent-form-clickthrough-collect-signature-and-record-timestamp-of-clicking-agree
    "Jun 3 at 6:50 PM", "Jun 3 at 8:00 PM" // https://trello.com/c/BhAdNNtf/2-add-signature-drawing-on-consent-form-clickthrough-collect-signature-and-record-timestamp-of-clicking-agree
    "Jun 10 at 5:45 PM", "Jun 10 at 7:56 PM" // https://trello.com/c/BhAdNNtf/2-add-signature-drawing-on-consent-form-clickthrough-collect-signature-and-record-timestamp-of-clicking-agree
    "Jun 11 at 5:24 PM", "Jun 11 at 6:43 PM" // https://trello.com/c/BhAdNNtf/2-add-signature-drawing-on-consent-form-clickthrough-collect-signature-and-record-timestamp-of-clicking-agree
    "Jun 14 at 2:10 PM", "Jun 14 at 2:41 PM" // https://trello.com/c/BhAdNNtf/2-add-signature-drawing-on-consent-form-clickthrough-collect-signature-and-record-timestamp-of-clicking-agree
    "Jun 11 at 6:14 PM", "Jun 11 at 6:43 PM" // https://trello.com/c/9K8pdKA3/15-add-things-to-the-sign-form-popup
    "Jun 14 at 2:10 PM", "Jun 14 at 2:32 PM" // https://trello.com/c/9K8pdKA3/15-add-things-to-the-sign-form-popup
    "Jun 11 at 6:43 PM", "Jun 11 at 8:27 PM" // https://trello.com/c/xF3b4hk8/6-add-auto-logout-functionality-make-time-period-configurable
    "Jun 14 at 2:22 PM", "Jun 14 at 2:32 PM" // https://trello.com/c/xF3b4hk8/6-add-auto-logout-functionality-make-time-period-configurable
    "Jun 11 at 5:24 PM", "Jun 11 at 8:45 PM" // https://trello.com/c/KEpj5jmj/16-working 
    "Jun 14 at 10:15 AM", "Jun 14 at 10:41 AM" // https://trello.com/c/KEpj5jmj/16-working 
    "Jun 14 at 11:20 AM", "Jun 14 at 2:42 PM" // https://trello.com/c/KEpj5jmj/16-working 
    
]
|> Seq.map(Tuple2.map(replace "at " ", 2019 "))
|> Seq.map(Tuple2.map DateTime.Parse)
|> Seq.sortBy fst
//|> Dump
|> Seq.map(fun (start,fin) -> Spans.dateTimeSpan start fin)
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