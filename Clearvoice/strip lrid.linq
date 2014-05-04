<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Namespace>System.Web</Namespace>
</Query>

//var input=LINQPad.Util.ReadLine<string>("What shall we encode?");
var input=System.Windows.Forms.Clipboard.GetText();
Regex.Replace(input,"lrid=\"[0-9]+\"",string.Empty).Dump();