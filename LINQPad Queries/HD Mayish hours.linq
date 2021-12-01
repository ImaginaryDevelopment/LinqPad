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
    "May 10 at 9:23 PM","May 10 at 10:49 PM" //"https://trello.com/c/Oi7PCbM8/16-change-cache-file-save-location-to-a-folder-that-is-not-specific-to-the-logged-in-user"
    "May 10 at 10:49 PM","May 10 at 10:49 PM" // "https://trello.com/c/Oi7PCbM8/16-change-cache-file-save-location-to-a-folder-that-is-not-specific-to-the-logged-in-user"
    "May 8 at 12:13 AM", "May 8 at 12:13 AM" // https://trello.com/c/3gf7Ae8v/15-unbreaking-the-breaking-change
    "May 1 at 5:41 PM", "May 1 at 5:51 PM" // https://trello.com/c/GUy5J3YH/2-message-to-pass-tablet-back-to-participant-is-no-longer-showing-up-bug
    "May 3 at 9:49 AM", "May 3 at 10:03 AM" // https://trello.com/c/GUy5J3YH/2-message-to-pass-tablet-back-to-participant-is-no-longer-showing-up-bug
    "May 1 at 6:37 PM", "May 1 at 6:43 PM" // https://trello.com/c/fl4DqHrl/7-add-participant-notes-freetext-field-at-end-of-questionnaire-add-field-to-db-and-store-during-event-upload
    "May 1 at 7:14 PM", "May 1 at 8:19 PM" // https://trello.com/c/fl4DqHrl/7-add-participant-notes-freetext-field-at-end-of-questionnaire-add-field-to-db-and-store-during-event-upload
    "May 2 at 12:25 PM", "May 2 at 2:32 PM" // https://trello.com/c/fl4DqHrl/7-add-participant-notes-freetext-field-at-end-of-questionnaire-add-field-to-db-and-store-during-event-upload
    "May 3 at 8:40 AM", "May 3 at 9:44 AM" // https://trello.com/c/fl4DqHrl/7-add-participant-notes-freetext-field-at-end-of-questionnaire-add-field-to-db-and-store-during-event-upload
    "May 1 at 6:15 PM", "May 1 at 6:29 PM" // https://trello.com/c/u5A184vH/3-add-column-on-event-selection-page-that-shows-client-name
    "May 1 at 6:43 PM", "May 1 at 7:11 PM" // https://trello.com/c/u5A184vH/3-add-column-on-event-selection-page-that-shows-client-name
    "May 1 at 6:29 PM", "May 1 at 6:37 PM" // https://trello.com/c/ogbGUMQk/4-tapping-download-event-without-an-internet-connection-should-show-the-user-an-error-message-please-connect-to-wifi-to-download-e
    "May 1 at 7:40 PM", "May 1 at 8:19 PM" // https://trello.com/c/ogbGUMQk/4-tapping-download-event-without-an-internet-connection-should-show-the-user-an-error-message-please-connect-to-wifi-to-download-e
    "May 2 at 12:24 PM", "May 2 at 12:25 PM" // https://trello.com/c/ogbGUMQk/4-tapping-download-event-without-an-internet-connection-should-show-the-user-an-error-message-please-connect-to-wifi-to-download-e
    "May 1 at 5:51 PM", "May 1 at 6:15 PM" // https://trello.com/c/v20dTLdd/1-critical-value-popup-font-size-too-small
    "Apr 27 at 11:20 PM", "Apr 27 at 11:55 PM" // https://trello.com/c/jeFhVjcW/8-remove-login-window-and-instead-log-in-the-user-with-the-windows-user-account-and-pass-that-account-name-back-to-the-server-when
    "Apr 29 at 10:53 PM", "Apr 30 at 12:29 AM" // https://trello.com/c/jeFhVjcW/8-remove-login-window-and-instead-log-in-the-user-with-the-windows-user-account-and-pass-that-account-name-back-to-the-server-when
    "Apr 30 at 8:38 PM", "Apr 30 at 9:02 PM" //  https://trello.com/c/jeFhVjcW/8-remove-login-window-and-instead-log-in-the-user-with-the-windows-user-account-and-pass-that-account-name-back-to-the-server-when
    "May 1 at 8:20 AM", "May 1 at 12:33 PM" // https://trello.com/c/jeFhVjcW/8-remove-login-window-and-instead-log-in-the-user-with-the-windows-user-account-and-pass-that-account-name-back-to-the-server-when
    "May 3 at 10:04 AM", "May 3 at 11:28 AM" // https://trello.com/c/hwMQLyjs/12-delivery
    "May 11 at 12:35 PM", "May 11 at 12:36 PM" // https://trello.com/c/UhLmZt4M/17-move-downloading-to-chunks
    "May 11 at 1:04 PM", "May 11 at 4:33 PM" // https://trello.com/c/UhLmZt4M/17-move-downloading-to-chunks
    "May 11 at 4:41 PM", "May 11 at 5:38 PM" // https://trello.com/c/UhLmZt4M/17-move-downloading-to-chunks
    "May 12 at 5:11 PM", "May 12 at 6:37 PM" // https://trello.com/c/xCIFFcO8/19-multi-user-data-problems-ask-me-about-this 
    "May 13 at 9:49 PM", "May 13 at 9:55 PM" // https://trello.com/c/e09XQQeD/13-allow-user-to-see-the-fact-that-a-download-is-processing
    "May 13 at 4:12 PM", "May 13 at 5:34 PM" // https://trello.com/c/O8lJfMvV/20-troubleshooting-and-adding-diagnostics
    "May 13 at 6:13 PM", "May 13 at 7:22 PM" // https://trello.com/c/O8lJfMvV/20-troubleshooting-and-adding-diagnostics
    "May 13 at 8:48 PM", "May 13 at 9:19 PM" // https://trello.com/c/O8lJfMvV/20-troubleshooting-and-adding-diagnostics
    "May 19 at 2:15 PM", "May 19 at 7:04 PM" // https://trello.com/c/V4sJESi5/18-fasting-question-on-summary-page-is-sometimes-not-holding-the-answer-value-from-the-previous-pages-see-description
    "May 20 at 9:30 PM", "May 20 at 11:21 PM" // https://trello.com/c/V4sJESi5/18-fasting-question-on-summary-page-is-sometimes-not-holding-the-answer-value-from-the-previous-pages-see-description
    "May 21 at 1:11 PM", "May 21 at 3:53 PM" // https://trello.com/c/V4sJESi5/18-fasting-question-on-summary-page-is-sometimes-not-holding-the-answer-value-from-the-previous-pages-see-description
    "May 21 at 4:13 PM", "May 21 at 6:47 PM" // https://trello.com/c/V4sJESi5/18-fasting-question-on-summary-page-is-sometimes-not-holding-the-answer-value-from-the-previous-pages-see-description
    "May 22 at 11:04 AM", "May 22 at 11:04 PM" // https://trello.com/c/V4sJESi5/18-fasting-question-on-summary-page-is-sometimes-not-holding-the-answer-value-from-the-previous-pages-see-description
    "May 23 at 9:37 AM", "May 23 at 1:10 PM" // https://trello.com/c/V4sJESi5/18-fasting-question-on-summary-page-is-sometimes-not-holding-the-answer-value-from-the-previous-pages-see-description
    
    
    
    
    
    
    
    //"", "" // 
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