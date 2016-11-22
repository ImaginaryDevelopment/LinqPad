<Query Kind="FSharpProgram">
  
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

// watch charges and journal entries for appt
open Microsoft.FSharp.Linq.NullableOperators
let serialize (o:obj) = Newtonsoft.Json.JsonConvert.SerializeObject(o, Newtonsoft.Json.JsonSerializerSettings(ReferenceLoopHandling=Newtonsoft.Json.ReferenceLoopHandling.Ignore))
module Option =
    let ofNull x = if isNull x then None else Some x
        
let dc = new TypedDataContext()
let apptId = 130
let appt = dc.Appointments.Single(fun a -> a.AppointmentID = apptId)
appt.Dump()
let pt = 
    //let ptOrNull = 
        dc.Patients.SingleOrDefault(fun pt -> pt.PatientID = appt.AppointmentPatientID) 
        |> Option.ofNull
        |> Option.get
//    if isNull ptOrNull then
//        None
//    else Some ptOrNull
    
let ptAccount = pt.Account |> Option.ofNull |> Option.get

let ptAccountId = ptAccount.AccountID

let getBalanceOrDie (journalEntries:#seq<JournalEntry>) = 
    journalEntries 
    |> Seq.sumBy (fun je -> if je.CreditAccountID = ptAccountId then je.Amount elif je.DebitAccountID = ptAccountId then je.Amount * -1m else failwithf "bad journal entry %A for paId %i" je ptAccountId  )
    

//type ApptEntries = {Charges: Charge list; JournalEntries: JournalEntry list; }
type ApptMeta = {Balance: decimal; ApptId: int; PtAccountId: int; PatientId: int; PatientInfoIdOpt: int option}

let getStats () = 
    let charges = dc.Charge.Where(fun ch -> ch.AppointmentID ?= apptId) |> List.ofSeq
    let chargeIds = charges |> Seq.map(fun c -> c.ChargeID) |> Array.ofSeq
    let journalEntries = dc.JournalEntries.Where(fun je -> je.AppointmentID ?= apptId || (je.ChargeID.HasValue && chargeIds.Contains(je.ChargeID.Value))) |> List.ofSeq
    
    //let ppi = if p.PayerProfileInfoID.GetValueOrDefault() > 0 then dc.PayerProfileInfo.First(fun ppi -> ppi.PayerProfileInfoID = p.PayerProfileInfoID.Value) |> Some else None
    // cached types must be defined somewhere more stable than current query (MyExtensions or a referenced dll) http://forum.linqpad.net/discussion/13/caching-things-between-execution-runs-but-same-linqpad-process-same-open-tab
    serialize {Balance = getBalanceOrDie journalEntries; ApptId= apptId; PatientId=pt.PatientID; PatientInfoIdOpt= pt.PatientInfoID |> Option.ofNullable; PtAccountId= ptAccountId  },
        charges,journalEntries
    
let cachedStats = Util.Cache (Func<_>(getStats))

let cMeta, cCharges, cJes= cachedStats
let nMeta, nCharges, nJes = getStats()
if cMeta <> nMeta then 
    Util.HorizontalRun(true, cMeta,nMeta).Dump()
else
    nMeta.Dump()
Util.HorizontalRun(false, cCharges,nCharges).Dump()
Util.HorizontalRun(false, cJes, nJes).Dump()
