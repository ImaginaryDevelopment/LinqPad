<Query Kind="FSharpExpression" />

let setter1 = @"
            this._AlternateBillingAddress = gpi.AlternateBillingAddress.GetValueOrDefault();
            this._CompanyID = gpi.CompanyID ?? 0;
            this._ContactFax = gpi.ContactFax;
            this._ContactName = gpi.ContactName;
            this._ContactNotes = gpi.ContactNotes;
            this._ContactPhoneNumber = gpi.ContactPhone;
            this._DeductibleRemFam = gpi.DeductibleRemFam.GetValueOrDefault();
            this._DeductibleRemInd = gpi.DeductibleRemInd.GetValueOrDefault();
            this._GroupNumber = gpi.GroupNumber;
            //this._GuarantorProfileID 
            this._GuarantorProfileInfoID = gpi.GuarantorProfileInfoID;
            this._IsInsurance = gpi.IsInsurance;
            this._IsPtAssignedBenefits = gpi.IsPtAssignedBenefits.GetValueOrDefault();
            this._IsPtConsentsRelease = gpi.IsPtConsentsRelease.GetValueOrDefault();
            this._LastUpdateDate = gpi.LastUpdateDate;
            this._LastUpdateUser = gpi.LastUpdateUser.GetValueOrDefault();
            //this._LatnameFirstname
            this._NamedAddress1 = gpi.NamedAddress1;
            this._NamedAddress2 = gpi.NamedAddress2;
            this._NamedCity = gpi.NamedCity;
            this._NamedCompany = gpi.NamedCompany;
            this._NamedDOB = gpi.NamedDOB;
            this._NamedFirstName = gpi.NamedFirstName;
            this._NamedInsuredType = gpi.NamedInsuredType;
            this._NamedLastName = gpi.NamedLastName;
            this._NamedPhoneNumber = gpi.NamedPhoneNumber;
            this._NamedSSN = gpi.NamedSSN;
            this._NamedState = gpi.NamedState;
            this._NamedSystemID = gpi.NamedSystemID.GetValueOrDefault();
            this._NamedZip = gpi.NamedZip;
            this._Notes = gpi.Notes;
            //this._PatientBackImage
            //this._PatientID
            //this._PatientImage
            //this._PatientImageBackData
            //this._PatientImageData
            //this._PatientInsuranceCardID
            //this._Payer
            this._PayerTypeID = gpi.PayerTypeID.GetValueOrDefault();
            this._PaymentCopay = gpi.PaymentCopay.GetValueOrDefault();
            this._PaymentCopayIsPercentage = gpi.PaymentCopayIsPercentage.GetValueOrDefault();
            this._PaymentFamilyDeductible = gpi.PaymentFamilyDeductible.GetValueOrDefault();
            this._PaymentIndividualDeductible = gpi.PaymentIndividualDeductible.GetValueOrDefault();
            this._PaymentIsReferralRequired = gpi.PaymentIsReferralRequired.GetValueOrDefault();
            this._PaymentOutofPocketFamily = gpi.PaymentOutofPocketFamily.GetValueOrDefault();
            this._PaymentOutofPocketIndividual = gpi.PaymentOutofPocketIndividual.GetValueOrDefault();
            this._PaymentOutofPocketRemainingFamily = gpi.PaymentOutofPocketRemainingFamily.GetValueOrDefault();
            this._PaymentOutofPocketRemainingIndividual = gpi.PaymentOutofPocketRemainingIndividual.GetValueOrDefault();
            this._PlanDescriptor = gpi.PlanDescriptor;
            this._PolicyNumber = gpi.PolicyNumber;
            //this._ProfileTypeAndPayer
            this._RelationshipID = gpi.RelationshipID.GetValueOrDefault();
            this._RxBin = gpi.RxBin;
            this._RxGroup = gpi.RxGroup;
            this._RxPcn = gpi.RxPcn;
            this._Type = gpi.Type;";

let setter2 = """ var guarantorProfile = new GuarantorProfileDataModel()
            {
                GuarantorProfileID = reader.ReadValueMap("GuarantorProfileID", Convert.ToInt32) ?? 0,
                GuarantorProfileInfoID = reader.ReadValueMap("GuarantorProfileInfoID", Convert.ToInt32) ?? 0,
                PatientID = reader.ReadValueMap("GuarantorProfilePatientID", Convert.ToInt32) ?? 0,
                IsInsurance = reader.ReadValueMap("IsInsurance", Convert.ToBoolean) ?? false,
                Type = reader.ReadToString("Type") ?? string.Empty,
                CompanyID = reader.ReadValueMap("CompanyID", Convert.ToInt32) ?? 0,
                PayerTypeID = reader.ReadValueMap("PayerTypeID", Convert.ToInt32) ?? 0,
                PolicyNumber = reader.ReadToString("PolicyNumber") ?? string.Empty,
                GroupNumber = reader.ReadToString("GroupNumber") ?? string.Empty,
                PlanDescriptor = reader.ReadToString("PlanDescriptor") ?? string.Empty,
                RxPcn = reader.ReadToString("RxPcn") ?? string.Empty,
                RxBin = reader.ReadToString("RxBin") ?? string.Empty,
                RxGroup = reader.ReadToString("RxGroup") ?? string.Empty,
                RelationshipID = reader.ReadValueMap("RelationshipID", Convert.ToInt32) ?? 0,
                AlternateBillingAddress = reader.ReadValueMap("AlternateBillingAddress", Convert.ToBoolean) ?? false,
                NamedSystemID = reader.ReadValueMap("NamedSystemID", Convert.ToInt32) ?? 0,
                NamedInsuredType = reader.ReadToString("NamedInsuredType") ?? string.Empty,
                NamedFirstName = reader.ReadToString("NamedFirstName") ?? string.Empty,
                NamedLastName = reader.ReadToString("NamedLastName") ?? string.Empty,
                NamedAddress1 = reader.ReadToString("NamedAddress1") ?? string.Empty,
                NamedAddress2 = reader.ReadToString("NamedAddress2") ?? string.Empty,
                NamedCity = reader.ReadToString("NamedCity") ?? string.Empty,
                NamedState = reader.ReadToString("NamedState") ?? string.Empty,
                NamedZip = reader.ReadToString("NamedZip") ?? string.Empty,
                NamedSSN = reader.ReadToString("NamedSSN") ?? string.Empty,
                NamedCompany = reader.ReadToString("NamedCompany") ?? string.Empty,
                NamedPhoneNumber = reader.ReadToString("NamedPhoneNumber") ?? string.Empty,
                PaymentCopay = reader.ReadValueMap("PaymentCopay", Convert.ToDouble) ?? 0,
                PaymentIndividualDeductible = reader.ReadValueMap("PaymentIndividualDeductible", Convert.ToDouble) ?? 0,
                PaymentOutofPocketIndividual = reader.ReadValueMap("PaymentOutofPocketIndividual", Convert.ToDouble) ?? 0,
                PaymentOutofPocketRemainingIndividual = reader.ReadValueMap("PaymentOutofPocketRemainingIndividual", Convert.ToDouble) ?? 0,
                PaymentFamilyDeductible = reader.ReadValueMap("PaymentFamilyDeductible", Convert.ToDouble) ?? 0,
                PaymentOutofPocketFamily = reader.ReadValueMap("PaymentOutofPocketFamily", Convert.ToDouble) ?? 0,
                PaymentOutofPocketRemainingFamily = reader.ReadValueMap("PaymentOutofPocketRemainingFamily", Convert.ToDouble) ?? 0,
                PaymentIsReferralRequired = reader.ReadValueMap("PaymentIsReferralRequired", Convert.ToBoolean) ?? false,
                Notes = reader.ReadToString("Notes") ?? string.Empty,
                IsPtAssignedBenefits = reader.ReadValueMap("IsPtAssignedBenefits", Convert.ToBoolean) ?? false,
                IsPtConsentsRelease = reader.ReadValueMap("IsPtConsentsRelease", Convert.ToBoolean) ?? false,
                DeductibleRemInd = reader.ReadValueMap("DeductibleRemInd", Convert.ToDouble) ?? 0,
                DeductibleRemFam = reader.ReadValueMap("DeductibleRemFam", Convert.ToDouble) ?? 0,
                ContactName = reader.ReadToString("ContactName") ?? string.Empty,
                ContactFax = reader.ReadToString("ContactFax") ?? string.Empty,
                ContactNotes = reader.ReadToString("ContactNotes") ?? string.Empty,
                ContactPhoneNumber = reader.ReadToString("ContactPhone") ?? string.Empty
            };

guarantorProfile.Payer.PayerID = guarantorProfile.companyid;
if (reader["NamedDOB"] != DBNull.Value)
	guarantorProfile.NamedDOB = Convert.ToDateTime(reader["NamedDOB"]);
"""

let varNames1 = Regex.Matches(setter1,@"\sthis\._(\w+) = ") |> Seq.cast<Match> |> Seq.map (fun m -> m.Groups.[1].Value)
varNames1

let varNames2 = Regex.Matches(setter2, @"(\w+) = ")|> Seq.cast<Match> |> Seq.map (fun m -> m.Groups.[1].Value)
let onlyIn1 = varNames1 |> Seq.filter (fun v1 -> Seq.exists(fun v2 -> v2=v1) varNames2 = false)
let onlyIn2 = varNames2 |> Seq.filter (fun v2 -> Seq.exists(fun v1 -> v1=v2) varNames1 = false)
onlyIn1,onlyIn2
