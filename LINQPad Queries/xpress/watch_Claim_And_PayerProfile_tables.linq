<Query Kind="FSharpProgram">
</Query>

open Microsoft.FSharp.Linq.NullableOperators

let dc = new TypedDataContext()
let apptId = 207
let ptId = dc.Appointments.Single(fun a -> a.AppointmentID = apptId).AppointmentPatientID
dc.Patients.Where(fun p -> p.PatientID =ptId).Dump()
type PayerProfileStats = {PPCount:int; PpiCount:int; PPId: int option; PrimaryPpiId: int option; PrimaryPpiOpt: PayerProfileInfo option; ClaimPrimaryPayerInfoId: int option; ClaimSecondaryPayerInfoId: int option}

let getStats () = 
    let appt = dc.Appointments.First(fun a -> a.AppointmentID = apptId)
    let ptId = appt.AppointmentPatientID

    let claim = dc.Claims.Single(fun a -> a.AppointmentID ?= apptId)
    let primaryPpiIdOpt = 
        let primary = dc.PayerProfileInfo.SingleOrDefault(fun p -> p.PayerProfileInfoID =? claim.PrimaryPayerProfileInfoID)
        if isNull primary then None else Some primary
    let payerProfileInfoId = primaryPpiIdOpt |> Option.map (fun p -> p.PayerProfileInfoID)
    let pPId = primaryPpiIdOpt |> Option.map(fun p -> p.PayerProfileID) |> Option.bind Option.ofNullable
    //let ppi = if p.PayerProfileInfoID.GetValueOrDefault() > 0 then dc.PayerProfileInfo.First(fun ppi -> ppi.PayerProfileInfoID = p.PayerProfileInfoID.Value) |> Some else None
    {   PPCount=dc.PayerProfiles.Count()
        PpiCount=dc.PayerProfileInfo.Count()
        PPId=pPId
        PrimaryPpiId = payerProfileInfoId
        PrimaryPpiOpt = primaryPpiIdOpt
        ClaimPrimaryPayerInfoId=claim.PrimaryPayerProfileInfoID |> Option.ofNullable 
        ClaimSecondaryPayerInfoId= claim.SecondaryPayerProfileInfoID |> Option.ofNullable }
    
let cachedStats = Util.Cache (Func<_>(getStats))

cachedStats.Dump("cached")
getStats().Dump("new")