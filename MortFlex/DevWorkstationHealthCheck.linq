<Query Kind="Statements" />

//dev env health check
var windir=System.Environment.ExpandEnvironmentVariables(@"%windir%");
var path = System.IO.Path.Combine(windir,@"SysWOW64\inetsrv\WMFToPDF.dll");
Debug.Assert(System.IO.File.Exists(path),"Document Printing will fail");
path = System.IO.Path.Combine(windir,"formset.ini");
Debug.Assert(System.IO.File.Exists(path),"involves printing?");
