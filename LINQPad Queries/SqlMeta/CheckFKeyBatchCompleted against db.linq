<Query Kind="FSharpExpression">
  
</Query>

let batchText = """ALTER TABLE [dbo].[Appointments] WITH CHECK CHECK CONSTRAINT [FK_Appointments_ADMIT_ADMITFACILITY_AdmitFacilityID];

ALTER TABLE [dbo].[AppointmentDiagnoses] WITH CHECK CHECK CONSTRAINT [FK_AppointmentDiagnoses_Appointments_AppointmentID];

ALTER TABLE [dbo].[AppointmentDiagnoses] WITH CHECK CHECK CONSTRAINT [FK_AppointmentDiagnoses_Diagnoses_DiagnosesID];

ALTER TABLE [dbo].[Appointments] WITH CHECK CHECK CONSTRAINT [FK_Appointments_GuarantorTypes_GuarantorTypeId];

ALTER TABLE [dbo].[Appointments] WITH CHECK CHECK CONSTRAINT [FK_Appointments_PCP_PcpId];

ALTER TABLE [dbo].[Claims] WITH CHECK CHECK CONSTRAINT [FK_Claims_Appointments_AppointmentId];

ALTER TABLE [dbo].[Appointments] WITH CHECK CHECK CONSTRAINT [FK_Appointments_Patients_PatientId];

ALTER TABLE [dbo].[Appointments] WITH CHECK CHECK CONSTRAINT [FK_Appointments_PatientsInfo_PatientInfoId];

ALTER TABLE [dbo].[Appointments] WITH CHECK CHECK CONSTRAINT [FK_Appointments_ADMIT_ADMITSTATUS_ADMITSTATUSID];

ALTER TABLE [dbo].[ChargeProfiles] WITH CHECK CHECK CONSTRAINT [FK_ChargeProfiles_DefaultServiceFacilityID_ServiceFacilities_ServiceFacilityId];

ALTER TABLE [dbo].[CodeChargeMappings] WITH CHECK CHECK CONSTRAINT [FK_CodeChargeMappings_ChargeProfiles_ChargeProfileID];

ALTER TABLE [dbo].[Claims] WITH CHECK CHECK CONSTRAINT [FK_Claims_POS_PlacesofService_PlaceofServiceID];

ALTER TABLE [dbo].[Claims] WITH CHECK CHECK CONSTRAINT [FK_Claims_Users_UserId];

ALTER TABLE [dbo].[Claims] WITH CHECK CHECK CONSTRAINT [FK_Claims_PrimaryPayerProfileInfoID_PayerProfileInfo_PayerProfileInfoID];

ALTER TABLE [dbo].[Claims] WITH CHECK CHECK CONSTRAINT [FK_Claims_SecondaryPayerProfileInfoID_PayerProfileInfo_PayerProfileInfoID];

ALTER TABLE [dbo].[Claims] WITH CHECK CHECK CONSTRAINT [FK_Claims_TertiaryPayerProfileInfoID_PayerProfileInfo_PayerProfileInfoID];

ALTER TABLE [dbo].[Claims] WITH CHECK CHECK CONSTRAINT [FK_Claims_ThirdPartyPayerProfileInfoID_PayerProfileInfo_PayerProfileInfoID];

ALTER TABLE [dbo].[Claims] WITH CHECK CHECK CONSTRAINT [FK_Claims_WorkmansCompPayerProfileInfoID_PayerProfileInfo_PayerProfileInfoID];

ALTER TABLE [dbo].[XCharge] WITH CHECK CHECK CONSTRAINT [FK_XCharge_FacilityId_Facilities_FacilityId];

ALTER TABLE [dbo].[Facilities] WITH CHECK CHECK CONSTRAINT [FK_Facilities_AccountId_Account_AccountID];

ALTER TABLE [dbo].[Facilities] WITH CHECK CHECK CONSTRAINT [FK_Facilities_FacilityPlaceofServiceID_PlacesofService_PlacesofServiceID];

ALTER TABLE [dbo].[AuditLog] WITH CHECK CHECK CONSTRAINT [FK_AuditLog_Patients_PatientID];

ALTER TABLE [dbo].[Patients] WITH CHECK CHECK CONSTRAINT [FK_Patients_AccountID_Account_AccountID];

ALTER TABLE [dbo].[PatientsInfo] WITH CHECK CHECK CONSTRAINT [FK_PatientsInfo_ReferralSource_ReferralSource_Id];

ALTER TABLE [dbo].[PatientsInfo] WITH CHECK CHECK CONSTRAINT [FK_PatientsInfo_ReferralSourceSub_ReferralSourceSubs_IdSubId];

ALTER TABLE [dbo].[PatientsInfo] WITH CHECK CHECK CONSTRAINT [FK_PatientsInfo_AccountID_Account_AccountID];

ALTER TABLE [dbo].[PatientsInfo] WITH CHECK CHECK CONSTRAINT [FK_PatientsInfo_GuarantorID_Patients_PatientID];

ALTER TABLE [dbo].[PayerProfile] WITH CHECK CHECK CONSTRAINT [FK_PayerProfile_PatientID_Patients_PatientID];

ALTER TABLE [dbo].[PayerProfileInfo] WITH CHECK CHECK CONSTRAINT [FK_PayerProfileInfo_PayerID_Payers_PayerID];

ALTER TABLE [dbo].[PayerProfileInfo] WITH CHECK CHECK CONSTRAINT [FK_PayerProfileInfo_PayerProfileID_PayerProfile_PayerProfileID];

ALTER TABLE [dbo].[Statements] WITH CHECK CHECK CONSTRAINT [FK_Statements_Payers_PayerID];

ALTER TABLE [dbo].[Payers] WITH CHECK CHECK CONSTRAINT [FK_Payers_AccountID_Account_AccountID];

ALTER TABLE [dbo].[Payers] WITH CHECK CHECK CONSTRAINT [FK_Payers_AdjustmentAccountID_Account_AccountID];

ALTER TABLE [dbo].[AuditLog] WITH CHECK CHECK CONSTRAINT [FK_AuditLog_AuditUser_UserID];

ALTER TABLE [dbo].[UserPasswordArchive] WITH CHECK CHECK CONSTRAINT [FK_UserPasswordArchive_UserId_Users_UserId];

ALTER TABLE [dbo].[ClaimPaperworkItem] WITH CHECK CHECK CONSTRAINT [FK_ClaimPaperworkItem_ClaimID_Claims_ClaimID];

ALTER TABLE [dbo].[ClaimPaperworkItem] WITH CHECK CHECK CONSTRAINT [FK_ClaimPaperworkItem_PaperworkTypeId_PaperworkType_PaperworkTypeID];

ALTER TABLE [Accounts].[JournalEntry] WITH CHECK CHECK CONSTRAINT [FK_JournalEntry_DebitAccountID_Account_AccountID];

ALTER TABLE [Accounts].[JournalEntry] WITH CHECK CHECK CONSTRAINT [FK_JournalEntry_PatientID_Patients_PatientID];

ALTER TABLE [Accounts].[JournalEntry] WITH CHECK CHECK CONSTRAINT [FK_JournalEntry_PaymentID_Payment_PaymentID];

ALTER TABLE [Accounts].[JournalEntry] WITH CHECK CHECK CONSTRAINT [FK_JournalEntry_PaymentItemID_PaymentItem_PaymentItemID];

ALTER TABLE [Accounts].[JournalEntry] WITH CHECK CHECK CONSTRAINT [FK_JournalEntry_AppointmentID_Appointments_AppointmentID];

ALTER TABLE [Accounts].[JournalEntry] WITH CHECK CHECK CONSTRAINT [FK_JournalEntry_ChargeID_Charge_ChargeID];

ALTER TABLE [Accounts].[JournalEntry] WITH CHECK CHECK CONSTRAINT [FK_JournalEntry_UserID_Users_UserID];

ALTER TABLE [Accounts].[JournalEntry] WITH CHECK CHECK CONSTRAINT [FK_JournalEntry_CreditAccountID_Account_AccountID];

ALTER TABLE [Accounts].[Account] WITH CHECK CHECK CONSTRAINT [FK_Account_AccountTypeId_AccountType_AccountTypeId];

ALTER TABLE [Accounts].[Account] WITH CHECK CHECK CONSTRAINT [FK_Account_PayerID_Payers_PayerID];

ALTER TABLE [Payments].[PaymentItem] WITH CHECK CHECK CONSTRAINT [FK_PaymentItem_PaymentItemStatusId_PaymentItemStatus_PaymentItemStatusId];

ALTER TABLE [Payments].[PaymentItem] WITH CHECK CHECK CONSTRAINT [FK_PaymentItem_PaymentID_Payment_PaymentID];

ALTER TABLE [Payments].[PaymentItem] WITH CHECK CHECK CONSTRAINT [FK_PaymentItem_ChargeID_Charge_ChargeID];

ALTER TABLE [Payments].[PaymentItem] WITH CHECK CHECK CONSTRAINT [FK_PaymentItem_PaymentItemTypeId_PaymentItemType_PaymentItemTypeId];

ALTER TABLE [dbo].[Charge] WITH CHECK CHECK CONSTRAINT [FK_Charge_AppointmentID_Appointments_AppointmentID];

ALTER TABLE [dbo].[Charge] WITH CHECK CHECK CONSTRAINT [FK_Charge_PatientID_Patients_PatientID];

ALTER TABLE [dbo].[Payment] WITH CHECK CHECK CONSTRAINT [FK_Payment_UserID_Users_UserID];

ALTER TABLE [dbo].[Payment] WITH CHECK CHECK CONSTRAINT [FK_Payment_PaymentMethodId_PaymentMethod_PaymentMethodId];

ALTER TABLE [dbo].[Payment] WITH CHECK CHECK CONSTRAINT [FK_Payment_PaymentStatusId_PaymentStatus_PaymentStatusId];

ALTER TABLE [dbo].[Payment] WITH CHECK CHECK CONSTRAINT [FK_Payment_CCItemID_CCItem_CCItemID];

ALTER TABLE [dbo].[Payment] WITH CHECK CHECK CONSTRAINT [FK_Payment_PaymentTierId_PaymentTier_PaymentTierId];

ALTER TABLE [dbo].[Payment] WITH CHECK CHECK CONSTRAINT [FK_Payment_PatientID_Patients_PatientID];

ALTER TABLE [dbo].[Payment] WITH CHECK CHECK CONSTRAINT [FK_Payment_PaymentTypeId_PaymentType_PaymentTypeId];

ALTER TABLE [dbo].[Payment] WITH CHECK CHECK CONSTRAINT [FK_Payment_PayerID_Payers_PayerID];

"""
let fkeys = 
    batchText
    |> fun s -> Regex.Matches(s,@"\[(FK.*)\]")
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Groups.[1].Value)
    |> List.ofSeq

let existingFKeys =
    dc.sys.Sysobjects.Where(fun o -> o.Type = "F") 
    |> List.ofSeq

fkeys
|> Seq.map (fun name -> 
    let found = existingFKeys |> Seq.tryFind(fun f -> f.Name = name) |> Option.isSome 
    name,found)