<Query Kind="Program" />

void Main()
{
	//Remote mmc
	var options = new[]{"lusrmgr","services","eventvwr","compmgmt"};
	
	var selected=Util.ReadLine("service?","compmgmt",options);
	
	Process.Start("mmc",selected);
}
IDictionary<string,Computers> aliasDictionary= new Dictionary<string,Computers>(){ 
	{"buildServer",Computers.svrrbidevbuild01}
	,{"tfs",Computers.svrrbidevsvc01}
	,{"laptop",Computers.osxe654014f8ty1}
	};
// Define other methods and classes here
public enum Computers
{
	//build server
	svrrbidevbuild01,
	//tfs server
	svrrbidevsvc01,
	[System.ComponentModel.Description("Ray's laptop")]
	OSXXPS264HTQ1,
	//[System.ComponentModel.Description("gtpm-init1-dit")]
	osxe654014f8ty1,
}