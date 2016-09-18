<Query Kind="FSharpExpression" />

// break insert up, mate columns to values
// assumes the data has no ')' or ','
// todo: rework using fparsec
let text = """insert into dbo.PayerProfileInfo(AlternateBillingAddress,ContactFax,ContactName,ContactNotes,ContactPhone,DeductibleRemFam,DeductibleRemInd,GroupNumber,IsInsurance,IsPtAssignedBenefits,IsPtConsentsRelease,LastUpdateDate,LastUpdateUser,NamedAddress1,NamedAddress2,NamedCity,NamedCompany,NamedDOB,NamedFirstName,NamedInsuredType,NamedLastName,NamedPhoneNumber,NamedSSN,NamedState,NamedSystemID,NamedZip,Notes,PayerID,PayerProfileID,PayerTypeID,PaymentCopay,PaymentCopayIsPercentage,PaymentFamilyDeductible,PaymentIndividualDeductible,PaymentIsReferralRequired,PaymentOutofPocketFamily,PaymentOutofPocketIndividual,PaymentOutofPocketRemainingFamily,PaymentOutofPocketRemainingIndividual,PlanDescriptor,PolicyNumber,RelationshipID,RxBin,RxGroup,RxPcn,Type) values ('False',null,null,null,null,'0','0',null,'True','False','False','10/13/2015 11:07:54',3,'124 SAFSDF',null,'ASDFSF',null,'01/18/1978 00:00:00','BRANDON',null,'DIMPERIO',null,null,'AK',1,'32065',null,2,2,1,'0','False','0','0','False','0','0','799.39','313.38',null,'XIL903510069',1,null,null,null,'PRIMARY INSURANCE');select SCOPE_IDENTITY()"""

let m = Regex.Match(text,@"insert into (.*)\((\w+,?)+\)\s+values\s+\(([^,\)]+,?)+\)")
let getGroupCaptureValues (i:int) (m:Match) = m.Groups.[i].Captures |> Seq.cast<Capture> |> Seq.map (fun c -> c.Value) |> List.ofSeq
let columns,values = m |> getGroupCaptureValues 2 , m|> getGroupCaptureValues 3
Debug.Assert(columns.Length = values.Length,"Columns to values didn't match up")
//columns.Captures,values.Captures

Seq.zip columns values
|> Seq.map (fun (c,v) -> c.TrimEnd([| ',' |] ), v.TrimEnd([| ',' |]))

