<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Namespace>System.Web</Namespace>
</Query>

//var input=LINQPad.Util.ReadLine<string>("What shall we encode?");
var input=System.Windows.Forms.Clipboard.GetText();
"<pre class='brush: csharp'>".Dump();
System.Net.WebUtility.HtmlEncode(input).Dump();
"</pre>".Dump();
input.Dump("Raw");
System.Net.WebUtility.HtmlEncode(input).Dump("System.Net.WebUtility.HtmlEncode");
System.Net.WebUtility.UrlEncode(input).Dump("UrlEncode");
HttpUtility.HtmlEncode(input).Dump("System.Web.HttpUtility.HtmlEncode");
HttpUtility.HtmlAttributeEncode(input).Dump("HtmlAttribute");