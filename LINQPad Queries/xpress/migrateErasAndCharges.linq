<Query Kind="FSharpProgram" />

let payments = 
    dc.Payments.Where(fun py ->
    (py.Appointment.AppointmentBillingStage = Nullable 5 || py.Appointment.Claims.Any(fun c -> c.ClaimStatus = Nullable 5)) 
    && py.Appointment.Charges.Count > 0
    //&& py.Eras.Count < 1
    ).ToList()
let toEra = payments |> Seq.filter(fun x -> x.EraPayment = null)
let toEraToCharge = payments |> Seq.filter(fun x -> x.EraPayment <> null && x.EraPayment.EraToCharges.Count = 0)
if toEra |> Seq.exists(fun _ -> true) then
    toEra
    |> Seq.map(fun py -> 
        EraPayment(EraPaymentID=py.PaymentID, DeliveryMethod="Migrated", DeliveryName="Migrated", Name="Migrated", 
            CreatedUtc = DateTime.UtcNow)
    )
    |> dc.EraPayments.InsertAllOnSubmit
    dc.SubmitChanges()

toEraToCharge
|> Seq.choose(fun x -> 
    match x.EraPayment with
    | null -> None
    | era -> x.Appointment.Charges.Select(fun ch -> era.EraPaymentID,ch.ChargeID).ToList() |> Some
)
|> Seq.concat
|> Seq.map(fun (eraId,chId) ->
    EraToCharge(EraPaymentID=eraId,ChargeID=chId,CreatedUtc=DateTime.UtcNow)
)
|> Dump
|> dc.EraToCharges.InsertAllOnSubmit
|> ignore

dc.SubmitChanges()