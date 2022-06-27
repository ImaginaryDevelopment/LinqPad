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
    
    "Dec 3 at 2:19 PM", "Dec 3 at 2:39 PM", "https://trello.com/c/AqLugpsG/1-how-can-i-generate-a-user-password-hash-based-on-a-plaintext-value"
    "Dec 3 at 2:38 PM", "Dec 3 at 2:54 PM", "https://trello.com/c/mb5SAoKN/4-signature-popup-shows-debug-resolution-always-should-not-show"
    "Dec 3 at 2:38 PM", "Dec 3 at 2:54 PM", "https://trello.com/c/P3zKrzL9/3-double-result-rows-being-created-async"
    "Dec 3 at 3:47 PM", "Dec 3 at 4:40 PM", "https://trello.com/c/gafdKtKW/5-when-clicking-save-and-the-double-entry-fields-dont-match-show-popup-and-redden-fields-where-the-values-dont-match"
    "Dec 5 at 9:39 AM", "Dec 5 at 9:53 AM", "https://trello.com/c/gafdKtKW/5-when-clicking-save-and-the-double-entry-fields-dont-match-show-popup-and-redden-fields-where-the-values-dont-match"
    "Dec 5 at 9:53 AM", "Dec 5 at 10:45 AM", "https://trello.com/c/o10w0xDE/6-final-summary-page-move-the-back-to-list-button-further-away-from-sign-too-close-for-errors-make-the-sign-button-green"
    "Dec 7 at 10:08 AM", "Dec 7 at 10:09 AM", "https://trello.com/c/o10w0xDE/6-final-summary-page-move-the-back-to-list-button-further-away-from-sign-too-close-for-errors-make-the-sign-button-green"
    "Dec 7 at 10:09 AM", "Dec 7 at 11:41 AM", "https://trello.com/c/C9LF5Cod/7-upload-async-results-when-program-is-restarted-for-ones-that-werent-able-to-upload-during-input"
    // working card
    "Dec 3 at 2:18 PM", "Dec 3 at 4:40 PM", null
    "Dec 5 at 9:39 AM", "Dec 5 at 11:03 AM", null
    "Dec 7 at 10:08 AM", "Dec 7 at 11:41 AM", null
//    "Oct 21 at 8:00 PM", "Oct 21 at 10:28 PM", null
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