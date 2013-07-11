<Query Kind="Program" />

void Main()
{
	//Remote mmc
	var options = new[]{"lusrmgr","services","eventvwr","compmgmt"};
	
	var selected=Util.ReadLine("service?","compmgmt",options);
	
	var computerNames=new HashSet<string>(){ "Test"};
}
IDictionary<string,Computers> aliasDictionary= new Dictionary<string,Computers>(){ 
	{"softwareDrive",Computers.b0024811fefeb}
	,{"Nbkif5uDesktop",Computers.F0024811FF013}
	,{"Nbkif5uLaptop",Computers.e68b599fb81ae}
	,{"GtpmDit", Computers.gtpm_init1_dit}
	};
// Define other methods and classes here
public enum Computers
{
	//software drive
	b0024811fefeb,
	F0024811FF013,
	e68b599fb81ae,
	[System.ComponentModel.Description("gtpm-init1-dit")]
	gtpm_init1_dit,
}
