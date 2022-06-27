<Query Kind="Expression">
  <Connection>
    <ID>6d9036c3-dd08-4e55-8c56-0d7d31c5ebfc</ID>
    <Persist>true</Persist>
    <Server>prog7-pc</Server>
    <SqlSecurity>true</SqlSecurity>
    <Database>ApplicationDatabase</Database>
    <UserName>xpu10</UserName>
    <IncludeSystemObjects>true</IncludeSystemObjects>
  </Connection>
</Query>

PatientIdentifications 
	//.Where(pid => pid.PatientsInfo.Any(pi => pi.PatientID == 1 ))
	//.Take(2)
	.Select(pid => 
		new { 
			Last = pid.PatientInfo.FirstOrDefault().LastName,
			PiId = pid.PatientIdentificationID, 
			XPM = pid.PatientInfo.Select(pi => pi.PatientID).FirstOrDefault(),
			FaceScanData = Util.Image(pid.FaceScanData), 
			FrontScanData = pid.FrontScanData,
			Back = pid.BackScanData
		})
	.ToList() // force into memory, so that the Util.OnDemand stuff below works =(
	.Select(x =>
		new
		{
			Last = x.Last,
			PIId = x.PiId,
			XPM= x.XPM,
			Face = x.FaceScanData,
			Front = Util.OnDemand("Front" + x.PiId, () => Util.Image(x.FrontScanData)),
			Back = Util.OnDemand("Back" + x.PiId, () => Util.Image(x.Back))
		}
	)