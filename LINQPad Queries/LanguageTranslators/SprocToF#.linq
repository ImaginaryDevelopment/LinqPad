<Query Kind="Statements" />

//sproc to F# params

var text = @"@ClaimID int = null,
       @POS int = null,
       @ExtID varchar(255) = null,
       @PatientID int = null,
       @FacilityID int = null,
       @AppointmentID int = null,
       @PrimaryGuarantorProfileInfoID int = null,
       @SecondaryGuarantorProfileInfoID int = null,
       @TertiaryGuarantorProfileInfoID int = null,
       @WorkmansCompGuarantorProfileInfoID int = null,
	   @ThirdPartyGuarantorProfileInfoID int = null,
       @Lmp datetime = null,
       @SubmissionDelayReason int = null,
       @DisabilityStartDate datetime = null,
       @DisabilityEndDate datetime = null,
       @LastWorked datetime = null,
       @ReturnToWork datetime = null,
       @IsAccident bit = null,
       @AccidentDate datetime = null,
       @IllnessDate datetime = null,
       @HospitalizationFromDate datetime =null,
       @HospitalizationToDate datetime =null,
       @CurrentIllnessDate datetime = null,
       @FirstIllnessDate datetime = null,
       @AccidentType varchar(255) = null,
       @AccidentState varchar(5) = null,
       @AuthException int = null,
       @ReferralNumber varchar(255) = null,
       @PriorAuth varchar(255) = null,
       @NoteToPayer varchar(MAX) = null,
       @CurrentLevel varchar(255) = null,
       @MaxLevel varchar(255) = null,
       @ClaimAmount decimal(18,2) = null,
       @ClaimCharges varchar(255) = null,
       @AvailDocs varchar(MAX) = null,
       @ClaimType varchar(10) = null,
       @EmergencySvc int = null,
       @Submitted bit = null,
       @CreateDateTime datetime = null,
       @SubmitDateTime datetime = null,
       @ReceivedDateTime datetime = null,
       @ClaimFreq int = null,
       @Adjudicated int = null,
       @CobState int = null,
       @ClaimStatus int = null,
       @RefProviderFirstName varchar(50) = null,
       @RefProviderLastName varchar(50) = null,
       @RefProviderNPI varchar(20) = null,
       @MedicareICN varchar(50) = null,
       @IsPregnant bit = null,
	   @ClaimFilingIndicatorCodeID int =nulll,
	   @ServiceFacilityID int = null,
	   @Claim837Type varchar(2) = null";
	   
	   
var regex = new Regex(@"@(\w+)");
var names = regex.Matches(text).Cast<Match>()
	.Select(m =>m.Groups[1].Value)
	
	;
var camelized = names.Select(n => n.EndsWith("ID")? n.BeforeLast("ID") + "Id": n).Select(n =>  new string(  n.Skip(1).Prepend(Char.ToLower( n.First())).ToArray()));


String.Join(" ",camelized).Dump("for args");
String.Join(",",camelized).Dump("for tuple");
String.Join(",", names.Select(n => "claim."+n)).Dump("for call");