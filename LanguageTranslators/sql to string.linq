<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Namespace>System.Web</Namespace>
</Query>

//sql to c# string
//var input=LINQPad.Util.ReadLine<string>("What shall we reformat?");
//TODO:
//bool convertComments=false;
var input=System.Windows.Forms.Clipboard.GetText();
input.SplitLines().Select(l=>"\""+l+"\"").Delimit(Environment.NewLine+"+").Dump();