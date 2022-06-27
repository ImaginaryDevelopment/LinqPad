<Query Kind="FSharpProgram" />

// phase 10 hours
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
    "Aug 21 at 7:43 PM", "Aug 21 at 7:50 PM", "https://trello.com/c/GN9xYaEr/4-client-reports-that-a-result-was-entered-with-an-ldl-value-of-5-which-is-outside-of-allowed-range-see-if-something-exists-that-w"
    "Sep 12 at 6:21 PM", "Sep 12 at 6:41 PM", "https://trello.com/c/aIRJ6ZZR/21-bug-adding-a-person-from-the-app-manually-sets-a-flag-in-the-participant-table-to-indicate-it-was-added-manually-it-sets-this-fi"
    "Aug 21 at 7:51 PM", "Aug 21 at 10:25 PM", "https://trello.com/c/xI2JCOmj/6-allow-upload-of-event-even-with-0-results-this-will-update-the-checkout-row-and-delete-the-event-from-the-tablet-but-not-enter-a"
    "Aug 21 at 10:25 PM", "Aug 21 at 10:55 PM", "https://trello.com/c/btHfws5Z/5-when-downloading-event-change-user-entered-event-id-to-be-the-foreigneventid-and-not-the-eventid-key"
    "Aug 21 at 10:55 PM", "Aug 21 at 10:58 PM", "// https://trello.com/c/049fypYY/8-add-input-validation-for-weight-79-501-inclusive"
    "Aug 23 at 1:59 PM", "Aug 23 at 2:29 PM", "https://trello.com/c/VlTuY7l9/7-change-the-checkbox-order-view-to-match-exactly-the-format-in-the-attached-image-for-this-question"
    "Aug 23 at 2:55 PM", "Aug 23 at 3:30 PM", "https://trello.com/c/2cThISXA/11-on-the-bio-entry-page-show-height-in-feet-and-inches-instead-of-just-inches"
    "Aug 23 at 3:31 PM", "Aug 23 at 4:46 PM", "https://trello.com/c/JrSjLJpi/13-when-adding-participant-manually-make-gender-and-member-id-required-fields"
    
    "Aug 23 at 2:30 PM", "Aug 23 at 2:44 PM", "https://trello.com/c/i9M1vIpT/9-the-results-confirmation-signature-should-be-on-the-review-summary-page-not-the-results-entry-page"
    "Sep 16 at 4:59 PM", "Sep 16 at 6:35 PM", "https://trello.com/c/i9M1vIpT/9-the-results-confirmation-signature-should-be-on-the-review-summary-page-not-the-results-entry-page"
    "Sep 23 at 4:15 PM", "Sep 23 at 4:26 PM", "https://trello.com/c/i9M1vIpT/9-the-results-confirmation-signature-should-be-on-the-review-summary-page-not-the-results-entry-page"
    
    "Sep 23 at 4:27 PM" , "Sep 23 at 4:37 PM", "https://trello.com/c/3Rzx3XOx/12-add-direct-button-back-to-participant-page-if-declined-is-selected-on-consent-back-to-list"
    
    "Aug 23 at 4:47 PM", "Aug 23 at 5:22 PM", "https://trello.com/c/t8fHxwKN/14-on-the-4-question-format-number-the-questions-1-4-instead-of-using-the-original-numbers"
    "Sep 23 at 4:37 PM", "Sep 23 at 5:08 PM", "https://trello.com/c/t8fHxwKN/14-on-the-4-question-format-number-the-questions-1-4-instead-of-using-the-original-numbers"
    
    "Sep 23 at 5:08 PM", "Sep 23 at 5:14 PM", "https://trello.com/c/HU1QAq4r/15-include-fasting-question-on-4-question-format-also-on-results-entry-page-ie-shows-it-twice-like-the-normal-way"
    "Sep 25 at 4:22 PM", "Sep 25 at 4:24 PM", "https://trello.com/c/HU1QAq4r/15-include-fasting-question-on-4-question-format-also-on-results-entry-page-ie-shows-it-twice-like-the-normal-way"
    
    "Sep 25 at 4:24 PM", "Sep 25 at 4:43 PM", "https://trello.com/c/3Qtuf6De/16-do-not-display-participant-list-values-only-populate-when-searched"
    // https://trello.com/c/a2riADld/18-signature-boxes-add-participants-name-and-dob-right-above-or-under-the-signature-boxes-such-that-the-participant-is-able-to-veri
    // https://trello.com/c/hHRfay6o/22-add-these-questions-to-the-top-of-the-biometric-entry-page-to-work-exactly-like-the-are-you-fasting-question
    
    // working card
    "Aug 21 at 7:43 PM", "Aug 21 at 10:59 PM", null
    "Aug 23 at 1:59 PM", "Aug 23 at 5:22 PM", null
    "Sep 12 at 6:21 PM", "Sep 12 at 6:47 PM", null
    "Sep 16 at 4:59 PM", "Sep 16 at 6:35 PM", null
    "Sep 23 at 4:14 PM", "Sep 23 at 5:14 PM", null
    "Sep 25 at 4:18 PM", "Sep 25 at 4:44 PM", null
    "Sep 27 at 11:06 AM", "Sep 27 at 12:19 PM", null
    "Sep 30 at 10:29 AM", "Sep 30 at 1:17 PM", null
    "Sep 30 at 1:48 PM", "Sep 30 at 5:17 PM", null
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