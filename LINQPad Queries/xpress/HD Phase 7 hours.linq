<Query Kind="FSharpProgram" />

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
    "Mar 19 at 5:27 PM", "Mar 19 at 7:53 PM" // https://trello.com/c/PS0UQ85I/6-move-height-weight-bmi-waist-circumference-boxes-to-the-top-of-the-list-of-biometrics-leave-cotinine-and-a1c-at-the-bottom
    "Mar 19 at 5:15 PM", "Mar 19 at 5:27 PM" // https://trello.com/c/Vj5xGNfD/1-reposition-logout-button-from-right-to-left-too-close-to-other-functional-buttons
    "Mar 21 at 4:47 PM", "Mar 21 at 5:07 PM" // https://trello.com/c/Vj5xGNfD/1-reposition-logout-button-from-right-to-left-too-close-to-other-functional-buttons
    "Mar 22 at 4:55 PM", "Mar 22 at 5:35 PM" // https://trello.com/c/FFSUCDGK/7-after-uploading-results-from-an-event-the-event-should-clear-itself-from-the-tablet-completely
    "Apr 1 at 5:24 PM", "Apr 1 at 6:02 PM" // https://trello.com/c/FFSUCDGK/7-after-uploading-results-from-an-event-the-event-should-clear-itself-from-the-tablet-completely
    "Mar 28 at 5:03 PM", "Mar 28 at 6:13 PM" // https://trello.com/c/FX2OBiQX/12-increase-size-of-checkboxes-radio-buttons-by-50-to-make-easier-to-tap
    "Mar 28 at 6:13 PM", "Mar 28 at 6:58 PM" // https://trello.com/c/SkJl3mdp/15-add-question-numbers-to-each-question-like-1-questiontext-etc
    "Mar 19 at 5:22 PM", "Mar 19 at 8:03 PM" // https://trello.com/c/XPG91o2g/2-if-certain-biometric-values-are-too-high-show-a-warning-to-the-user
    "Apr 1 at 2:18 PM","Apr 1 at 2:20 PM" // https://trello.com/c/YnJfzpnG/9-add-a-space-after-logged-in-as-to-be-logged-in-as-main
    "Apr 1 at 5:07 PM","Apr 1 at 5:14 PM" // https://trello.com/c/CPKJ2FyP/10-question-6-tobacco-radio-buttons-allow-selection-of-both-yes-and-no-they-are-obviously-mutually-exclusive-they-also-show-both-se
    "Apr 1 at 6:02 PM","Apr 1 at 6:21 PM" // https://trello.com/c/BrJ6LxwT/13-summary-page-increase-font-size-by-4-points
    "Apr 1 at 5:23 PM","Apr 1 at 5:23 PM" // https://trello.com/c/8DRr3Owx/8-limit-the-resolution-measurements-to-2-decimal-points-currently-showing-1024x76799999999
    "Mar 28 at 4:08 PM", "Mar 28 at 5:03 PM" // https://trello.com/c/3DLg5azS/17-if-this-question-configuration-is-selected-see-image-the-fasting-question-does-not-save
    "Apr 1 at 5:05 PM", "Apr 1 at 5:06 PM" // https://trello.com/c/3DLg5azS/17-if-this-question-configuration-is-selected-see-image-the-fasting-question-does-not-save
    "Mar 19 at 5:25 PM","Mar 19 at 8:03 PM" // https://trello.com/c/JuUbsEM6/3-skipped-question-confirmation-if-hra-question-is-skipped-throw-popup-window-to-confirm
    "Mar 21 at 5:07 PM", "Mar 21 at 7:02 PM" // https://trello.com/c/JuUbsEM6/3-skipped-question-confirmation-if-hra-question-is-skipped-throw-popup-window-to-confirm
    "Apr 1 at 1:25 PM", "Apr 1 at 1:49 PM" // https://trello.com/c/JuUbsEM6/3-skipped-question-confirmation-if-hra-question-is-skipped-throw-popup-window-to-confirm
    "Apr 1 at 5:07 PM", "Apr 1 at 5:14 PM" // https://trello.com/c/JuUbsEM6/3-skipped-question-confirmation-if-hra-question-is-skipped-throw-popup-window-to-confirm
    "Apr 2 at 8:11 AM", "Apr 2 at 8:54 AM" // https://trello.com/c/JuUbsEM6/3-skipped-question-confirmation-if-hra-question-is-skipped-throw-popup-window-to-confirm
    "Apr 4 at 7:30 PM", "Apr 4 at 7:38 PM" // https://trello.com/c/2NZ1gv5Y/11-change-skipped-question-warning-text
    "Apr 4 at 7:39 PM","Apr 4 at 7:47 PM" // https://trello.com/c/npUHZmpm/19-on-the-consent-screen-show-the-participant-name-and-dob-in-large-bold-text-centered-underneath-the-consent-statement
    "Apr 1 at 2:20 PM", "Apr 1 at 5:24 PM" // https://trello.com/c/NZaEdSWT/16-dob-masking
    "Apr 4 at 7:47 PM", "Apr 4 at 8:15 PM" // https://trello.com/c/NZaEdSWT/16-dob-masking
    "Apr 8 at 4:33 PM", "Apr 8 at 5:06 PM" // https://trello.com/c/mrJeFUBV/18-db-values-for-the-1-5-questions-are-in-reverse-scaling-order-for-example-mostly-high-fat-should-return-a-1-instead-of-3-for-alco
    
    
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