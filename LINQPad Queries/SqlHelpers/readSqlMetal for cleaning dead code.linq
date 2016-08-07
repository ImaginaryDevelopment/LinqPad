<Query Kind="FSharpProgram" />

//clean up based on output of sqlmetal

[<AutoOpen>]
module Helpers = 
    let flip f x1 x2 = f x2 x1
    let dumpt (t:string) x = x.Dump(t); x
    let after (delimiter:string) (text:string) = text.Substring(text.IndexOf(delimiter) + delimiter.Length)
    let before (delimiter:string) (text:string) = text.Substring(0,text.IndexOf(delimiter))
    let linq t f = Hyperlinq(Action(f),t)
    let splitLines (text:string ) = text.Split([| "\r\n"; "\n" |], StringSplitOptions.None)
type Railway<'tSuccess,'tError> =
    | Success of 'tSuccess
    | Failure of 'tError
    
// for testing a single line
let tryGetSqm1014 line = 
    let m = Regex.Match(line, "Warning : SQM1014: Unable to extract stored procedure '(.*)' from .*'(.*)'.*")
    if m.Success then
        Success(m.Groups.[1].Value, m.Groups.[2].Value,line)
    else Failure line
    
let mapSqm1014(sproc,invalidObjectName,fullLine) = sprintf "drop procedure %s" sproc, sproc, invalidObjectName, fullLine
// for getting all matches (multi-line)
let getSqm1014 text = 
    Regex.Matches(text,"Warning : SQM1014: Unable to extract stored procedure '(.*)' from .*'(.*)'.*$", RegexOptions.Multiline)
    |> Seq.cast<Match>
    |> Seq.map(fun m ->sprintf "drop procedure %s" m.Groups.[1].Value, m.Groups.[1].Value, m.Groups.[2].Value, m.Value)
    // linq "drop sproc" (fun () -> 
type SqlMetalMappedOutput = 
    | Sqm1014 of dropper:obj*sproc:string*invalidObjectName:string*fullLine:string
    | Unmatched of string
let mapOutput t = 
    let lines = splitLines t |> Seq.filter (String.IsNullOrEmpty>>not) |> Array.ofSeq
    //let sqm1014s = getSqm1014 t
    
    let remainder = 
        query{
            for l in lines do
            let sqm1014 = 
                match tryGetSqm1014 l with
                | Success x -> mapSqm1014 x|> Some
                | Failure _ -> None
            //where (isNull x)
            select (match sqm1014 with | Some (d,s,o,l) -> Sqm1014 (box d,s,o,l) | None -> Unmatched l)
        }
        |> Seq.sortBy(function | Unmatched _ -> true | _ -> false)
        |> Array.ofSeq
        //lines |> Seq.filter (fun l -> sqm1014s |> Seq.map(fun (_,_,_,ml) -> ml) |> Seq.exists(fun ml -> l <> ml))
    
    remainder
    |> Seq.choose(function | Unmatched _ -> None |Sqm1014(d,s,o,l) -> Some (d,s,o,l))
    |> dumpt "Sqm1014s"
    |> ignore
    
    remainder
    |> Seq.choose(function | Unmatched l -> Some l | _ -> None)
    |> dumpt "remainder"
    |> ignore
    

let sqlMetalOutput = """
     <Association Name="FK_Appointments_Patients_PatientId" Member="Patients" ThisKey="AppointmentPatientID" OtherKey="PatientID" Type="Patients" IsForeignKey="true" /> 
      <Association Name="FK_Appointments_Patients_PatientId" Member="Appointments" ThisKey="PatientID" OtherKey="AppointmentPatientID" Type="Appointments" DeleteRule="NO ACTION" /> 
Warning : SQM1014: Unable to extract stored procedure 'dbo.sp_upgraddiagrams' from SqlServer. Invalid object name 'dbo.dtproperties'. 
Warning : SQM1014: Unable to extract stored procedure 'dbo.uspClaimFilingIndicatorByIDGet' from SqlServer. Invalid object name 'ClaimFillingIndicator'. 
Warning : SQM1014: Unable to extract stored procedure 'dbo.uspCodesWithMappingsAndCustomsGet' from SqlServer. Invalid object name '#Temp'. 
Warning : SQM1014: Unable to extract stored procedure 'dbo.uspCodesWithMappingsCustomOnlyGet' from SqlServer. Invalid object name '#Temp'. 
Warning : SQM1014: Unable to extract stored procedure 'dbo.uspCodesWithMappingsGet1' from SqlServer. Invalid object name '#Temp'. 
Warning : SQM1014: Unable to extract stored procedure 'dbo.uspDeleteGuarantorProfileByProfileID' from SqlServer. Invalid object name 'GuarantorProfiles'. 
Warning : SQM1014: Unable to extract stored procedure 'dbo.uspFixCurrentPayerID' from SqlServer. Invalid column name 'PrimaryGuarantorProfileInfoID'. Invalid column name 'SecondaryGuarantorProfileInfoID'. Invalid column name 'TertiaryGuarantorProfileInfoID'. 
Warning : SQM1019: Unable to extract stored procedure 'dbo.uspGetDependentPatients' from SqlServer because its result set contains columns with the same name 'PatientInfoID'. 
Warning : SQM1014: Unable to extract stored procedure 'dbo.uspGetPayersID' from SqlServer. Invalid column name 'PrimaryGuarantorProfileInfoID'. Invalid column name 'SecondaryGuarantorProfileInfoID'. Invalid column name 'TertiaryGuarantorProfileInfoID'. 
Warning : SQM1014: Unable to extract stored procedure 'dbo.uspGuarantorProfileDel' from SqlServer. Invalid object name 'GuarantorProfiles'. 
Warning : SQM1014: Unable to extract stored procedure 'dbo.uspGuarantorProfileGet' from SqlServer. Invalid object name 'GuarantorProfiles'. 
Warning : SQM1014: Unable to extract stored procedure 'dbo.uspGuarantorProfileInfoInsUpd' from SqlServer. Invalid object name 'GuarantorProfilesInfo'. 
Warning : SQM1014: Unable to extract stored procedure 'dbo.uspGuarantorProfileInsUpd' from SqlServer. Invalid object name 'GuarantorProfiles'. 
Warning : SQM1014: Unable to extract stored procedure 'dbo.uspGuarantorProfilesByPatientGet' from SqlServer. Invalid object name 'GuarantorProfiles'. 
Warning : SQM1014: Unable to extract stored procedure 'dbo.uspInsuranceGet' from SqlServer. Invalid object name 'Insurance'. 
Warning : SQM1014: Unable to extract stored procedure 'dbo.uspPatientInfoTestInsUpd' from SqlServer. Invalid object name 'PatientInfoTest'. 
Warning : SQM1019: Unable to extract stored procedure 'dbo.uspPatientsGet' from SqlServer because its result set contains columns with the same name 'PatientInfoID'. 
Warning : SQM1019: Unable to extract stored procedure 'dbo.uspPatientsGetPatientID' from SqlServer because its result set contains columns with the same name 'PatientInfoID'. 
Warning : SQM1014: Unable to extract stored procedure 'dbo.uspPayerProfileInfoInsUpd' from SqlServer. Invalid column name 'CompanyID'. 
Warning : SQM1019: Unable to extract stored procedure 'dbo.uspPayerProfilesByPatientGet' from SqlServer because its result set contains columns with the same name 'PayerProfileInfoID'. 
Warning : SQM1014: Unable to extract stored procedure 'dbo.uspPayersTotalChargesGet' from SqlServer. Invalid column name 'PayerID'. Invalid column name 'PayerID'. Invalid column name 'TotalCharge'. Invalid column name 'PayerID'. 
Warning : SQM1014: Unable to extract stored procedure 'dbo.uspPayersTotalClaimsGet' from SqlServer. Invalid column name 'PrimaryGuarantorProfileInfoID'. Invalid column name 'SecondaryGuarantorProfileInfoID'. Invalid column name 'TertiaryGuarantorProfileInfoID'. Invalid column name 'WorkmansCompGuarantorProfileInfoID'. 
Warning : SQM1014: Unable to extract stored procedure 'dbo.uspsSupervisingProvidersGet' from SqlServer. Invalid column name 'IsActive'. 
Warning DBML1008: Mapping between DbType 'DateTime2(7) NOT NULL' and Type 'System.DateTime' in Column 'AuditDate' of Type 'AuditLog' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'SelfCharges' of Type 'UspAppointmentsByIDGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalCharges' of Type 'UspAppointmentsByIDGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalPayments' of Type 'UspAppointmentsByIDGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalInsurancePayments' of Type 'UspAppointmentsByIDGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalPatientPayments' of Type 'UspAppointmentsByIDGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalOtherPayments' of Type 'UspAppointmentsByIDGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'Balance' of Type 'UspAppointmentsByIDGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalPatientPayments' of Type 'UspAppointmentsGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalOtherPayments' of Type 'UspAppointmentsGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalInsurancePayments' of Type 'UspAppointmentsGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'SelfCharges' of Type 'UspAppointmentsGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalCharges' of Type 'UspAppointmentsGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalPayments' of Type 'UspAppointmentsGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'Balance' of Type 'UspAppointmentsGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'SelfCharges' of Type 'UspAppointmentsGetByBillingStageResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalCharges' of Type 'UspAppointmentsGetByBillingStageResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalPayments' of Type 'UspAppointmentsGetByBillingStageResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalInsurancePayments' of Type 'UspAppointmentsGetByBillingStageResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalPatientPayments' of Type 'UspAppointmentsGetByBillingStageResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalOtherPayments' of Type 'UspAppointmentsGetByBillingStageResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'Balance' of Type 'UspAppointmentsGetByBillingStageResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalPatientPayments' of Type 'UspAppointmentsGetForRCMResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalOtherPayments' of Type 'UspAppointmentsGetForRCMResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalInsurancePayments' of Type 'UspAppointmentsGetForRCMResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalCharges' of Type 'UspAppointmentsGetForRCMResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalPayments' of Type 'UspAppointmentsGetForRCMResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'Balance' of Type 'UspAppointmentsGetForRCMResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,0)' and Type 'System.Decimal' in Column '' of Type 'UspB2BEligibilityTriggersInsUpdResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,0)' and Type 'System.Decimal' in Column '' of Type 'UspClaimFilingIndicatorInsUpdResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'claimAmount' of Type 'UspGetBillingResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'claimAmount' of Type 'UspGetCompletedResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'credits' of Type 'UspGetPatientBillingResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'debits' of Type 'UspGetPatientBillingResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'balance' of Type 'UspGetPatientBillingResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'claimAmount' of Type 'UspGetPendingResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'claimAmount' of Type 'UspGetRejectedResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'claimAmount' of Type 'UspGetReviewResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'credits' of Type 'UspPatientBalanceGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'debits' of Type 'UspPatientBalanceGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'balance' of Type 'UspPatientBalanceGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'total' of Type 'UspPatientBalanceGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'insurancepayment' of Type 'UspPatientInsuranceByAppointmentIDGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,0)' and Type 'System.Decimal' in Column '' of Type 'UspPatientsInsUpdResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'PayerAmount' of Type 'UspPaymentByProviderReportResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'PatientAmount' of Type 'UspPaymentByProviderReportResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'Adjustment' of Type 'UspPaymentByProviderReportResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'Balance' of Type 'UspPaymentByProviderReportResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'SelfCharges' of Type 'UspRecentAppointmentsGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalCharges' of Type 'UspRecentAppointmentsGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalPayments' of Type 'UspRecentAppointmentsGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalInsurancePayments' of Type 'UspRecentAppointmentsGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalPatientPayments' of Type 'UspRecentAppointmentsGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'TotalOtherPayments' of Type 'UspRecentAppointmentsGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,2)' and Type 'System.Decimal' in Column 'Balance' of Type 'UspRecentAppointmentsGetResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,0)' and Type 'System.Decimal' in Column '' of Type 'UspSpecialProgramInsUpdResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,0)' and Type 'System.Decimal' in Column '' of Type 'UspStatementsInsUpdResult' may cause data loss when loading from the database. 
Warning DBML1008: Mapping between DbType 'Decimal(38,0)' and Type 'System.Decimal' in Column '' of Type 'UspxChargeInsUpdResult' may cause data loss when loading from the database. 
"""
sqlMetalOutput
|> mapOutput
|> Dump