<Query Kind="Statements">
  <Namespace>System.Security</Namespace>
  <Namespace>System.Net</Namespace>
</Query>

//psexec
//Util.GetPassword("xpressdomainBrandonD")
var autoCompletes = new[] { @"psexec \\tfs20102.xpress.domain -u xpressdomain\brandond net view", "net stop mssqlserver", "iisreset /restart"};
var toRun = Util.ReadLine("toRun?", autoCompletes[0], autoCompletes);
Process.Start(new ProcessStartInfo("cmd", @"/k "+toRun) { CreateNoWindow = false, UseShellExecute = true });
//also :
//net stat
// net stop mssqlserver
// iisreset /restart