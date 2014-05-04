<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Namespace>System.Web</Namespace>
</Query>

//C# to sql
//var input=LINQPad.Util.ReadLine<string>("What shall we reformat?");
var input =@"""select count(1) as total,ui.invitation_source_id as sourceId ""
                   + "" from [project].[User_Invitation] ui with(nolock) "" +
                   "" where ui.project_quota_id= @pQgId and ui.""+column+"" = @status "" +
                   "" group by ui.invitation_source_id, ui.prelimstatus=""";
				   
input=System.Windows.Forms.Clipboard.GetText();
input
	.SplitLines()
	.Select(a=>a.TrimStart().StartsWith("+\"")? a.After("\""):a)
	.Dump()
	.Select(a=>a.TrimStart().StartsWith("+")? a.After("+"):a)
	.Select(a=>a.TrimStart().StartsWith("\"")? a.After("\""):a)
	.Dump()
	.Select(a=>a.TrimEnd().EndsWith("+")?a.BeforeLast("+"):a)
	.Select(a=>a.TrimEnd().EndsWith("\"")?a.BeforeLast("\""):a)
	
	.Delimit(Environment.NewLine).Dump();