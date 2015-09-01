<Query Kind="Statements">
  <Connection>
    <ID>4e94eacc-a31d-4687-947b-e4c9804c895a</ID>
    <Persist>true</Persist>
    <Server>(local)</Server>
    <IncludeSystemObjects>true</IncludeSystemObjects>
    <Database>XPEncounter</Database>
    <ShowServer>true</ShowServer>
  </Connection>
  <Reference>C:\TFS\XpressCharts\dev\bin\ReactiveUI.dll</Reference>
  <Reference>C:\TFS\XpressCharts\dev\bin\Xpress.EDIS.Documents.dll</Reference>
  <Reference>C:\TFS\XpressCharts\dev\bin\Xpress.Foundation.dll</Reference>
  <Reference>C:\TFS\XpressCharts\PatientPortal\XpressPatient\XpressDal\bin\Debug\XpressDal.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Xpress.Foundation.DataModels</Namespace>
</Query>

Util.OnDemand("PatientEncounters",() =>PatientData.Where(pd => pd.Encounters.Any()).Select(pd => new{ pd.PatientID, Encounters = pd.Encounters.Select(e=> e.EncounterID)})).Dump();
var patientId = 2;
var encounterId = 5;
Util.OnDemand("get allergies", ()=>  uspDrFirstAllergiesGet(patientId,null,null)).Dump("uspDrFirstAllergiesGet");
Util.OnDemand("get lab values", () => uspLabValuesGet(1,null)).Dump("uspLabValuesGet"); 
var appDbCs = "Data Source=(local);Integrated Security=SSPI;Initial Catalog=XPApplication";
var encDbCs = "Data Source=(local);Integrated Security=SSPI;Initial Catalog=XPEncounter";
var existing = new Xpress.EDIS.Documents.CCD.CCD().CreateCCD(patientId,encounterId,
	new ConnectionStringDataModel()
	{ 
		PatientDataConnectionString= encDbCs + ";app=XpressLinqPad", 
		ApplicationConfigurationConnectionString= appDbCs + ";app=XpressLinqPad"
	}
);
var dbE = XpressDal.Dal.Db.getEncounterContext(encDbCs + ";app=Fsx");
var dbA = XpressDal.Dal.Db.getApplicationContext(appDbCs + ";app=Fsx");

var existingData = existing.DumpFormatted("existing").ToString();
Util.OnDemand("UspChartsByEncounterIDGet", ()=> dbE.UspChartsByEncounterIDGet(encounterId,"Physician").FirstOrDefault()).Dump("chart");
var fsharp = XpressDal.Ccd.CSharp.createChartStrategyLookupChart(dbE, dbA, patientId,encounterId);
var fsharpData = fsharp.DumpFormatted("fsharp").ToString();

Util.OnDemand("kdiff", () => 
	{
		var existingFile = existingData.ToTempFile();
		var fsharpFile = fsharpData.ToTempFile(); 
		var args = String.Join(" ",existingFile.RawPath.SurroundWith("\""),fsharpFile.RawPath.SurroundWith("\""),"--L1 existing","--L2 fsharp").Dump("args");
		try
		{
			Util.Cmd(@"C:\Program Files\KDiff3\kdiff3.exe",args,true);
		}
		catch (CommandExecutionException ex)
		{
			ex.Dump();
		}
	
		return "finished";
	}
).Dump();

Util.NewProcess = true;