<Query Kind="Statements" />

Environment.CurrentDirectory = "C:/php/interpreter";
Environment.CurrentDirectory.Dump();
Func<string,string> runPhpWithArgs = v => 
{
	try
	{	        
		return Util.Cmd("php","../project/processor.php "+ v).Materialize().Aggregate((s1,s2) => s1+","+s2);	
	}
	catch (Exception ex)
	{
		
		return ex.ToString();
	}
};	

Func<string,string> phpEncryptvalue =v => runPhpWithArgs ("\""+v+"\" ENC");
Func<int,string> phpTestRun = i => runPhpWithArgs(i.ToString());

phpEncryptvalue("(local)").Dump("enc!");

phpTestRun(0).Dump("testrun!");
// Util.Cmd("php","../project/processor.php 0");
