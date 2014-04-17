<Query Kind="Statements" />

//visibility states
var file=@"C:\Development\Products\CVS\Member\CVS.Member.Web\Pages\UserControls\RewardsControl.ascx";


var markup= System.IO.File.ReadAllText(file);
var code= System.IO.File.ReadAllText(file+".cs");
//markup.Dump(); //code changes missing reward_history_mobile_2 
//count should be 31 perhaps?
var codeChanges = Regex.Matches(code,@"(\w{4}[\w_]+)\.Visible\s*=\s*(.*);", RegexOptions.IgnoreCase).Cast<Match>().Select(m=>new{ctl=m.Groups[1].Value,val= m.Groups[2].Value}).Dump();//.Dump(); //non capture is (?:
var markupValues = Regex.Matches(markup,@"\s*<[\w:]+(?=.*\srunat=""server"")(?=.*\sid=""([a-z]\w+)"")(?=.*\sVisible=""(true|false)"")?.*\/?>", RegexOptions.IgnoreCase).Cast<Match>().Select(m=>new{name=m.Groups[1].Value,InitiallyVisible=m.Groups[2].Value.IsNullOrEmpty() || m.Groups[2].Value.IsIgnoreCaseMatch("true")}).Dump();

var q= from cc in codeChanges
		join mv in markupValues on cc.ctl equals mv.name
		select new{cc,mv};
		
		q.Dump();