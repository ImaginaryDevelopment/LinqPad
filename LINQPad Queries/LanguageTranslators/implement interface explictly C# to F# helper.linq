<Query Kind="FSharpProgram" />

// implicit to explicit interface implementation

let transformReadOnly t = // substitutions in regex helper page: https://msdn.microsoft.com/en-us/library/ewy2t5e0(v=vs.110).aspx
    Regex.Replace(t,@"(member x.(\w+): (Nullable<)?\w+(<\w+>)?(>)? =)( |\t)*\r\n", @"$1 x.$2", RegexOptions.Multiline)
    |> (fun t -> Regex.Replace(t, @"(member x.(\w+)\s*.*)(\r\n)+(^\s*and set)", @"$1 x.$2$3$4", RegexOptions.Multiline))

let transformSetters t =
    Regex.Replace(t,@"member (x.\w+)\s*.*\s*and set.*=", "$0 $1 <- v", RegexOptions.Multiline)
    
    
let text = """interface IAppointment with
        member x.AdmitFacilityID: Nullable<int> = 

        member x.AdmitStatus: string = 

        member x.AppointmentAccidentDate: Nullable<DateTime> = 

        member x.AppointmentAccidentID: Nullable<int> = 

        member x.AppointmentAccidentRelated: Nullable<bool> = 

        member x.AppointmentAccidentState: string = 

        member x.AppointmentBillingStage: Nullable<int> = 

        member x.AppointmentCheckInFlag: bool = 

        member x.AppointmentCheckInTime: Nullable<DateTime> = 

        member x.AppointmentCheckOutTime: Nullable<DateTime> = 

        member x.AppointmentEndTime: DateTime = 

        member x.AppointmentFacilityID: int = 

        member x.AppointmentForeignEHRID: Nullable<int> = 

        member x.AppointmentID: int<AppointmentId> = 

        member x.AppointmentLOS: string = 

        member x.AppointmentPatientID: int<PatientId> = 

        member x.AppointmentPatientInfoID: int = 

        member x.AppointmentPrimaryGuarantorType: string = 

        member x.AppointmentProviderScheduledID: Nullable<int> = 

        member x.AppointmentStartTime: DateTime = 

        member x.AppointmentStatus: string = 

        member x.AppointmentTypeID: int = 

        member x.IsChecked: Nullable<bool> = 

        member x.NotesToBiller: string = 

        member x.PresentingCondition: string = 

        member x.ReferralPcp: Nullable<int> = 


    interface IAppointmentRW with
        member x.AdmitFacilityID
            with get (): Nullable<int> = 

            and set (v: Nullable<int>): unit = 

        member x.AdmitStatus
            with get (): string = 

            and set (v: string): unit = 

        member x.AppointmentAccidentDate
            with get (): Nullable<DateTime> = 

            and set (v: Nullable<DateTime>): unit = 

        member x.AppointmentAccidentID
            with get (): Nullable<int> = 

            and set (v: Nullable<int>): unit = 

        member x.AppointmentAccidentRelated
            with get (): Nullable<bool> = 

            and set (v: Nullable<bool>): unit = 

        member x.AppointmentAccidentState
            with get (): string = 

            and set (v: string): unit = 

        member x.AppointmentBillingStage
            with get (): Nullable<int> = 

            and set (v: Nullable<int>): unit = 

        member x.AppointmentCheckInFlag
            with get (): bool = 

            and set (v: bool): unit = 

        member x.AppointmentCheckInTime
            with get (): Nullable<DateTime> = 

            and set (v: Nullable<DateTime>): unit = 

        member x.AppointmentCheckOutTime
            with get (): Nullable<DateTime> = 

            and set (v: Nullable<DateTime>): unit = 

        member x.AppointmentEndTime
            with get (): DateTime = 

            and set (v: DateTime): unit = 

        member x.AppointmentFacilityID
            with get (): int = 

            and set (v: int): unit = 

        member x.AppointmentForeignEHRID
            with get (): Nullable<int> = 

            and set (v: Nullable<int>): unit = 

        member x.AppointmentID
            with get (): int<AppointmentId> = 

            and set (v: int<AppointmentId>): unit = 

        member x.AppointmentLOS
            with get (): string = 

            and set (v: string): unit = 

        member x.AppointmentPatientID
            with get (): int<PatientId> = 

            and set (v: int<PatientId>): unit = 

        member x.AppointmentPatientInfoID
            with get (): int = 

            and set (v: int): unit = 

        member x.AppointmentPrimaryGuarantorType
            with get (): string = 
            and set (v: string): unit = 

        member x.AppointmentProviderScheduledID
            with get (): Nullable<int> = 
            and set (v: Nullable<int>): unit = 

        member x.AppointmentStartTime
            with get (): DateTime = 
            and set (v: DateTime): unit = 

        member x.AppointmentStatus
            with get (): string = 
            and set (v: string): unit = 

        member x.AppointmentTypeID
            with get (): int = 
            and set (v: int): unit = 

        member x.IsChecked
            with get (): Nullable<bool> = 
            and set (v: Nullable<bool>): unit = 

        member x.NotesToBiller
            with get (): string = 
            and set (v: string): unit = 

        member x.PresentingCondition
            with get (): string = 
            and set (v: string): unit = 

        member x.ReferralPcp
            with get (): Nullable<int> = 
            and set (v: Nullable<int>): unit =
            """
            
transformReadOnly text
|> transformSetters
|> Dump