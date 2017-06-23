<Query Kind="FSharpProgram" />

// new post era/eob rules
// we will use the payment table to feed bundles of paymentItems

module Domain = 
    [<Measure>] type PatientId
    [<Measure>] type VisitId
    [<Measure>] type ChargeId
    [<Measure>] type PaymentItemId                           
    [<Measure>] type PayerId
    
    type PaymentItemType = 
        | EraAdjustment of adjustmentCode:string
        | Payment
    type PatientRespType = | CoIns | CoPay | Deductible
    type PatientResp = decimal * PatientRespType
    type RemarkCode = string

    type ItemCommon = { ChargeId: int<ChargeId> // by proxy, should have apptId/patient id
//                        VisitId: int<VisitId>
//                        PatientId: int<PatientId>
                        PaymentItemType: PaymentItemType
                        Amount:decimal
                        PatientResp: PatientResp option
                        RemarkCode: RemarkCode option }
    type ItemType = 
        // valid manual item
        |Manual of Guid 
        // source: payment Table?
        |Auto of int<PaymentItemId>

    type PaymentItemPair = ItemType * ItemCommon
    
    type Era = { PayerId:int<PayerId>; Items: PaymentItemPair list; }
open Domain

// like schema but for UI level stuffs
module Interface = 
    type Searchable = 
    |Patient
    |Visit
    |Charge
    |AdjCode
    |RemarkCode
    // if the user has clicked to approve this item (marked it as editing-complete, ready to submit)
    type Approval= bool
    type Flight = 
        {   ChargeId:int<ChargeId> option
            VisitId: int<VisitId> option
            PatientId: int<PatientId> option
            PaymentItemType: PaymentItemType option
            Amount:decimal
            PatientResp:PatientResp option
            RemarkCode:RemarkCode option }
    
    type ItemInFlight = { ItemType:ItemType; Flight:Flight}
    
    // Target Amount is how much the era is supposed to total once we have all the items approved/edited
    type EraInFlight = {Era:Era; Items: (ItemInFlight * Approval) list; TargetAmount:decimal option; RemitDate:DateTime option;CheckOrTransactionNumber:string}

open Interface


module Commands = 
    type ItemAction= 
        | Add
        | Edit
        | AdjustAmount of decimal
        
    type EraAction = 
        | SelectItem of PaymentItemPair option
        
    type AppAction = 
        | OpenEra of EraInFlight
        | EraAct of EraAction
        | ItemAct of ItemAction
        | Print of string
open Commands


module Display =
    type Displayable<'T> = 
        | TDisp of x:'T
        | Any of obj
            
    let makeDisplay<'T> titleOpt (x:Displayable<'T>) = 
        let getDisplay (x:Displayable<'T>) = 
            match x with
                | Any x -> x
                | TDisp x -> box x
        let dc = 
            DumpContainer( getDisplay x)
        dc.Dump(titleOpt |> Option.getOrDefault String.Empty)
        getDisplay >> (fun x -> dc.Content <- x)
    type Prop<'T> = {Get: unit -> 'T; Set: 'T -> unit}
    let makeStatefulDisplay<'T> titleOpt x : Prop<'T>= 
        let mutable item : 'T = x
        let d = makeDisplay titleOpt (TDisp item)
        {   Get = (fun () -> item)
            Set = (fun x ->
                    item <- x
                    TDisp x |> d)
        }
open Display


let e = {   Era= 
                {   PayerId= 1<PayerId>
                    Items= [ ItemType.Auto -1<PaymentItemId>,{ ChargeId= -1<ChargeId>;Amount= 12.34m; PaymentItemType=Payment;RemarkCode=None; PatientResp= Some(34.65m,CoPay)  }
                ]}
            TargetAmount = None
            RemitDate = None
            CheckOrTransactionNumber = null
            Items = List.empty
        }
        
type SelectedItem = 
    |InFlight of ItemInFlight
    |PI of PaymentItemPair
    
let mp = MailboxProcessor.Start(fun inbox ->    
    let currentEra = makeStatefulDisplay (Some "Era") e
    let currentItem:Prop<SelectedItem option> = makeStatefulDisplay (Some "Item") None
    let rec messageLoop() = async{
        let! msg = inbox.Receive()
        match msg with
        | OpenEra flight -> currentEra.Set flight
        | EraAct (SelectItem pi) -> pi |> Option.map PI |> currentItem.Set
        | ItemAct Edit ->
            // map PI -> Inflight and save in place
            currentItem.Get()
            |> Option.iter(function
                |InFlight _ -> ()
                |PI(itemType,itemCommon) -> 
                    {ItemType=itemType;Flight= 
                        {   ChargeId=Some itemCommon.ChargeId
                            VisitId=None
                            PatientId=None
                            PaymentItemType= Some itemCommon.PaymentItemType
                            Amount = itemCommon.Amount
                            PatientResp = itemCommon.PatientResp 
                            RemarkCode = itemCommon.RemarkCode}
                    }
                    |> InFlight
                    |> Some
                    |> currentItem.Set
            )
        | ItemAct (AdjustAmount amount) ->
            currentItem.Get()
            |> Option.bind (
                function
                | PI _ -> None
                | InFlight item -> 
                    Some item 
            )
            |> Option.iter(fun item ->
                {item with Flight = {item.Flight with Amount=amount} }
                |> InFlight
                |> Some
                |> currentItem.Set
            )
//        | Print msg -> printfn "message is: %s" msg
        return! messageLoop()
    }
    messageLoop()
)

[   OpenEra e

]
|> Seq.iter mp.Post