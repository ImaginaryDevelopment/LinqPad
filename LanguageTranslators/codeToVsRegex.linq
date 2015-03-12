<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Namespace>System.Web</Namespace>
</Query>

//translate to regex
//var input=LINQPad.Util.ReadLine<string>("What shall we encode?");
var input=System.Windows.Forms.Clipboard.GetText();
input.Replace("(",@"\(").Replace(")",@"\)").Replace("[",@"\[").Replace("]",@"\]").Replace("?",@"\?").Replace(".",@"\.").Dump();
// cmd\.Parameters\.Add\("(?<name>\w+)", SqlDbType\..*\)\.Value = (.*);