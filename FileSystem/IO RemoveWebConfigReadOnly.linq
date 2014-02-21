<Query Kind="Statements" />

var servers=System.Environment.GetEnvironmentVariable("servers", EnvironmentVariableTarget.User).Split(';');
var sitwebDeploy=@"\\"+servers[0]+@"\c$\inetpub";
var location= System.IO.Directory.GetFiles(@"\\"+servers[0]+@"\c$\inetpub");

location.Dump();
var webconfig=location.First(e=>e.EndsWith("Web.config"));
var attributes=System.IO.File.GetAttributes(webconfig);
attributes.Dump();
if((attributes & FileAttributes.ReadOnly)== FileAttributes.ReadOnly)
	System.IO.File.SetAttributes(webconfig, attributes ^ FileAttributes.ReadOnly);