<Query Kind="Program">
  <Connection>
    <ID>4e94eacc-a31d-4687-947b-e4c9804c895a</ID>
    <Persist>true</Persist>
    <Server>(local)</Server>
    <IncludeSystemObjects>true</IncludeSystemObjects>
    <Database>PmRewriteApplicationDatabase</Database>
    <ShowServer>true</ShowServer>
  </Connection>
</Query>

void Main()
{
	Payers.Where(p => p.IsChecked == true).ToList().ForEach(p => p.IsChecked = false);
	SubmitChanges();
	var apptId = 126; // original source was 99, copy is 126
	var appt = Appointments.First(a=>a.AppointmentID==apptId).Dump("appt");
	var claim = Claims.FirstOrDefault(c=> c.AppointmentID==apptId).Dump("claim");
	var payerId = 2;
	var facilityId = 8; //used for charge profile auto-adjustments only
	Util.OnDemand("ChargeProfileInfo",() => ChargeProfiles.Where(cp=> cp.PayerID==payerId && cp.FacilityID== facilityId)).Dump();
	var thisApptCharges = this.Charges.Where(c => c.AppointmentID == apptId);

	// step 1, prepclaim:
	// important columns: codeId = CPT, Charge = charge ,  PayerId, PatientId, apptId, providerId, chargeDatetime, units, totalcharge = charge * units, 
	// non-important, but still populated = Allowed = Total, Adjusted = 0m
	// step x review:
	// adjusted = adjustment= deductible, allowed is modified based on paid not on the allowed remittance column, 
	
	var charges = new[] {
		new ChargeEoB{Cpt="96374", Charge=167.52m},
		new ChargeEoB{Cpt="96375", Charge=66m,Units=2},
		new ChargeEoB{Cpt="J7030", Charge=50m},
		new ChargeEoB{Cpt="99204", Charge=499.08m, AllowedAmount=160.5m, Deductible=160.5m,IsPtResp=true },
		new ChargeEoB{Cpt="84443", Charge=101.39m},
		new ChargeEoB{Cpt="80048", Charge=43.99m},
		new ChargeEoB{Cpt="85025", Charge=40.39m},
		new ChargeEoB{Cpt="J1200", Charge=6.96m},
		new ChargeEoB{Cpt="80076", Charge=42.48m},
		new ChargeEoB{Cpt="J1885", Charge=15.38m,Units=2},
		new ChargeEoB{Cpt="J0780", Charge=38.18m},
		new ChargeEoB{Cpt="81001", Charge=71.57m},
		new ChargeEoB{Cpt="81025", Charge=64.80m},
		new ChargeEoB{Cpt="36415", Charge=10.8m},
		new ChargeEoB{Cpt="99354", Charge=738.5m, AllowedAmount=738.5m, Deductible=738.5m, IsPtResp=true },

	};
	
	// make raw sql
	//charges.Select(c => "Insert into charges(codeid, charge,adjusted,allowed, payerid,patientId, apptid, providerid,chargedatetime,units,totalcharge) values ('" + c.Cpt + )
//	var q = from c in charges
//			join ch in thisApptCharges on c.Cpt equals ch.CodeID
//			select new { Charge = new { Local=c.Charge, Db=ch.Charge,Equal=c.Charge==ch.Charge}};
		
	//q.Dump();
		
	if (thisApptCharges.Any())
	{
		thisApptCharges.Dump("already has charges, not adding/duplicating");
		PaymentsTemps.Dump("paymentsTemps");
		return;
	}
	charges.Dump();
	var newPrepClaimCharges = charges.Select(ch => ToPrepClaimCharge(ch, payerId, appt.AppointmentPatientID, apptId, appt.AppointmentProviderScheduledID, DateTime.Now));
	Charges.InsertAllOnSubmit(newPrepClaimCharges);
	SubmitChanges();
	PaymentsTemps.Dump();
	PaymentsTemps.DeleteAllOnSubmit(PaymentsTemps);
	SubmitChanges();
}

// Define other methods and classes here
public class ChargeEoB
{
	public ChargeEoB()
	{
		Units = 1;
	}
	
	// prepclaim column
	public string Cpt { get; set; }
	// prepclaim column
	public decimal Charge { get; set; }
	// prepclaim column
	public int Units { get; set; }
	public decimal Total { get { return Charge * Units; } }
	public decimal AllowedAmount { get; set; }
	
	public decimal InsurancePayment { get { return AllowedAmount - Deductible /* or co ins or copay */; } }
	public decimal Adjustment { get { return Total - AllowedAmount; } }
	public decimal Deductible { get; set; }

	public bool Denied { get { return Total > 0 && AllowedAmount == 0m; } }
	public bool IsPtResp { get; set; }

}

public static Charges ToPrepClaimCharge(ChargeEoB ch, int? payerId, int? patientId, int? apptId, int? providerId, DateTime? chargeDateTime)
{
	return new Charges
	{
		CodeID = ch.Cpt,
		Charge = ch.Charge,
		Adjusted = 0m,
		Allowed = ch.Total,
		PayerID = payerId,
		PatientID = patientId,
		AppointmentID = apptId,
		ProviderID = providerId,
		ChargeDatetime = DateTime.Now,
		Units = ch.Units,
		TotalCharge = ch.Total
	};
}

public static Charges ToPrepClaimChargesExperimental(ChargeEoB ch, int? payerId, int? patientId, int? apptId, int? providerId, DateTime? chargeDateTime)
{
	return new Charges
	{
		CodeID = ch.Cpt,
		Charge = ch.Charge,
		//Adjusted = 0m,
		//Allowed = ch.Total,
		PayerID = payerId,
		PatientID = patientId,
		AppointmentID = apptId,
		ProviderID = providerId,
		ChargeDatetime = DateTime.Now,
		Units = ch.Units,
		TotalCharge = ch.Total
	};
}