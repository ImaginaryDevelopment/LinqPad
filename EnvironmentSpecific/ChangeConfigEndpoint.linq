<Query Kind="Program">
  <Reference>&lt;RuntimeDirectory&gt;\System.ComponentModel.DataAnnotations.dll</Reference>
  <Namespace>System.ComponentModel.DataAnnotations</Namespace>
</Query>

const string PshEnvironmentConfig="PSH_CONFIG_ENDPOINT";
const string PshEnvironmentFormat="http://{0}/ConfigurationService/Configuration.ashx";
void Main()
{
	var names=Enum.GetNames(typeof(PshConfigEndpoint));
	var current=Environment.GetEnvironmentVariable(PshEnvironmentConfig);
	
	var envDic=from e in names
		let value=(PshConfigEndpoint) Enum.Parse(typeof(PshConfigEndpoint),e)
		let display=value.GetType().GetField(e).GetCustomAttribute<DisplayAttribute>()
		let name= display!=null? display.Name:e
		let full=String.Format(PshEnvironmentFormat,name)
		let isCurrent=full==current
	select new{Key=e,Value=value,Name=name,Full=full,IsCurrent=isCurrent};
	
	envDic.Select (d => new{d.Key,d.Name,Full=Util.HighlightIf(d.Full,n=>d.IsCurrent)}).Dump("options for "+PshEnvironmentConfig);
	
	if(Util.ReadLine<bool>("Change value?"))
	{
		var newSetting=Util.ReadLine<PshConfigEndpoint>("New setting?",
			PshConfigEndpoint.Pdo,names.Select (n =>
				(PshConfigEndpoint) Enum.Parse(typeof(PshConfigEndpoint),n)));
		Environment.SetEnvironmentVariable(PshEnvironmentConfig,
		envDic.Single (d =>d.Value==newSetting).Full, EnvironmentVariableTarget.User);
	}
	
	
}
enum PshConfigEndpoint
{
	[Display(Name=@"jaxpdoappl1:8000")]
	Pdo,
	localhost
	
}
enum roots{
	[Display(Name=@"c:\projects\trunk\hpx")]
	Hpx
}
enum PshConfigDb{
	localhost,
}
// Define other methods and classes here
