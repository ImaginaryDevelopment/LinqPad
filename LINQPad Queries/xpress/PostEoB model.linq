<Query Kind="FSharpProgram" />

// data fetch auth is not working yet
// new post era/eob rules
// we will use the payment table to feed bundles of paymentItems

// can an item exist/validate without any of the following (manual or auto) (manual made era, or data fed)
// patientid, visitid, chargeid
// chargeId should feed amount box? or no?

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
    
module Items = 
    let beginEdit = 
        function
            |InFlight _ -> None
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
                
    let adjustAmount amount =
        function
        | PI _ -> None
        | InFlight item -> Some item 
        >> Option.map(fun item ->
            {item with Flight = {item.Flight with Amount=amount} }
            |> InFlight
            |> Some
        )
        
open Items
// let's hit the app instead of the db?
module DataBased = 
    open System.Net
//    let private dc = new TypedDataContext()
    let private baseUrl = "http://localhost:8081"
    type Address = 
        | Uri of System.Uri
        | Raw of String

    let inline postIt url (formParams:IDictionary<string,string>) = 
        // https://stackoverflow.com/questions/930807/login-to-website-via-c-sharp
        let formParams = formParams |> Seq.map (|KeyValue|) |> Seq.map (fun (k,v) -> sprintf "%s=%s" k (System.Uri.EscapeDataString v)) |> delimit "&"
        let req = 
            match url with
            | Uri x -> WebRequest.Create x
            | Raw s -> WebRequest.Create s
        req.ContentType <- "application/x-www-form-urlencoded"
        req.RequestUri.Dump("request uri")
        req.Method <- "POST"
        let bytes = Encoding.ASCII.GetBytes(formParams)
        formParams.Dump()
        req.ContentLength <- int64 bytes.Length
        (
            use os = req.GetRequestStream()
            os.Write(bytes, 0, bytes.Length)
            os.Flush()
        )
        let resp = 
            match req.GetResponse() with
            | :? HttpWebResponse as x -> x
            | x -> failwithf "response type %s is unexpected" (x.GetType().Name)
        (int resp.StatusCode, resp.StatusCode, resp.StatusDescription)
        |> Dump
        |> ignore
        match int resp.StatusCode with
        | 302 -> ()
        | x -> failwithf "expecting a 302 instead was %i" x
        resp.Cookies.Dump("cookies?!?")
        let content = 
            use stream = resp.GetResponseStream()
            use reader = new StreamReader(stream, Encoding.UTF8)
            reader.ReadToEnd()
        content.Dump("content")            
        resp.Headers.Dump("headers")
        
        
    let login () =
        let url = sprintf "%s/authentication/login" baseUrl |> Raw
        Map [   "Username", "main"
                "Password","1234"
        ]
        |> postIt url 
        
    let searchPatients = 
        function
        | null | "" ->
            String.Empty
        | x ->
            login()
            |> Dump
            |> ignore
            //or use WebRequest.Create()
            use wc = new System.Net.WebClient()
            wc.DownloadString(sprintf "%s/patientSearch/%s" baseUrl x)

()
DataBased.searchPatients "dimp" |> Dump |> ignore
let mp = MailboxProcessor.Start(fun inbox ->    
    let currentEra = makeStatefulDisplay (Some "Era") e
    let currentItem:Prop<SelectedItem option> = makeStatefulDisplay (Some "Item") None
    let rec messageLoop() = async{
        let! msg = inbox.Receive()
        match msg with
        | OpenEra flight -> currentEra.Set flight
        | EraAct (SelectItem pi) -> pi |> Option.map PI |> currentItem.Set
        | ItemAct Add ->
            currentItem.Get()
            |> function
                | Some _ -> ()
                | None ->
                    {   ItemType= Guid() |> Manual
                        Flight= {   ChargeId= None
                                    VisitId= None
                                    PatientId= None
                                    Amount= 0m
                                    RemarkCode=  None
                                    PatientResp= None
                                    PaymentItemType= None
                        }
                    }
                    |> InFlight
                    |> Some
                    |> currentItem.Set
        | ItemAct Edit ->
            // map PI -> Inflight and save in place
            currentItem.Get()
            |> Option.iter(beginEdit >> currentItem.Set)
        | ItemAct (AdjustAmount amount) ->
            currentItem.Get()
            |> Option.bind (adjustAmount amount)
            |> Option.iter currentItem.Set   
        | Print msg -> printfn "message is: %s" msg
        return! messageLoop()
    }
    messageLoop()
)

[   OpenEra e

]
|> Seq.iter mp.Post