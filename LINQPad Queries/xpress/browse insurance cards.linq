<Query Kind="Expression">
  <Connection>
    <ID>6d9036c3-dd08-4e55-8c56-0d7d31c5ebfc</ID>
    <Persist>true</Persist>
    <Server>prog7-pc</Server>
    <SqlSecurity>true</SqlSecurity>
    <Database>ApplicationDatabase</Database>
    <UserName>xpu10</UserName>
  </Connection>
</Query>

PatientInsuranceCardImages 
	.Where(pid => pid.PatientID == 1)
	//.Take(2)
	.Select(pid => 
		new { 
			PiId = pid.PatientInsuranceCardID,
			PayerId = pid.PayerID
			XPM = pid.PatientID,
			FaceScanData = Util.Image(pid.PatientImage), 
			Back = pid.PatientBackImage
		})
	.ToList() // force into memory, so that the Util.OnDemand stuff below works =(
	.Select(x =>
		new
		{
			PIId = x.PiId,
			PayerId = x.PayerId,
			XPM= x.XPM,
			Face = x.FaceScanData,
			Back = Util.OnDemand("Back" + x.PiId, () => Util.Image(x.Back))
		}
	)