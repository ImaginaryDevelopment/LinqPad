<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Namespace>System.Web</Namespace>
</Query>

//var input=LINQPad.Util.ReadLine<string>("What shall we encode?");
var input=System.Windows.Forms.Clipboard.GetText();
input.Replace("\\","\\\\").Replace("\"","\\\"").Dump("escape 1");
input.Replace("\"","\\\"").Dump("escape 2");
input.Replace("\"","\"\"").Dump("escape for c# verbatim string");
input.Replace("\\","/").Dump();

input.Dump("original");