<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Namespace>System.Web</Namespace>
</Query>

//translate to regex
//var input=LINQPad.Util.ReadLine<string>("What shall we encode?");
var input=System.Windows.Forms.Clipboard.GetText().Dump("input");
var output = input
	.Replace("(",@"\(")
	.Replace(")",@"\)")
	.Replace("[",@"\[")
	.Replace("]",@"\]")
	.Replace("?",@"\?")
	.Replace(".",@"\.")
	.Replace("+",@"\+")
	.Replace("\r",@"\r")
	.Replace("\n",@"\n")
	.Replace("|",@"\|")
	.Dump("initialConversion")
	;
// cmd\.Parameters\.Add\("(?<name>\w+)", SqlDbType\..*\)\.Value = (.*);

// awesomeness: \(reader\[(?<name>"\w+")\] == DBNull\.Value\) \? (.+) : Convert\.(\w+)\(reader\[\k<name>\]\)
// 	replacement: reader.ReadValueMap($3, Convert.$2) ?? $1

// toString: \(reader\[(?<name>"\w+")\] == DBNull\.Value\) \? (.*) : \(reader\[\k<name>\]\.ToString\(\){1,2}
// 	replacement: reader.ReadToString($2) ?? $1
// as: \(reader\[(?<name>"\w+")\] == DBNull\.Value\) \? null : reader\[\k<name>\] as ([^;]+)
// 	replacement: reader.ReadMap($2, o => o as $1)

// complex?: \(reader\[(?<name>"\w+")\] == DBNull\.Value \|\| reader\[\k<name>\]\.ToString\(\) == string\.Empty\) \? (.+) : Convert\.(\w+)\(reader\[\k<name>\]\)
// 	replacement: reader.ReadValueMapIf($3, o => o.ToString() != string.Empty, Convert.$2) ?? $1
bool doContinue = true;
while(doContinue){
	var suggestions = input.Contains("\"")? input.Replace("(",string.Empty).Replace(")",string.Empty).Split('"').ToArray() : Enumerable.Empty<string>();
	suggestions = suggestions.Concat(suggestions.Select(s=> '"' + s + '"'));
	var toCapture = Util.ReadLine("capture?",suggestions.Count() >1 ? suggestions.Skip(1).First() : string.Empty,suggestions);
	if(toCapture == string.Empty)
	{
		doContinue = false;
		break;
	}
	if(output.Contains(toCapture) == false)
	{
		toCapture.Dump("not found");
		continue;
	}
	var replaceDefault = toCapture.StartsWith("\"") && toCapture.EndsWith("\"") ?@"(?<name>""\w+"")" : @"(?<name>\w+)" ;
	var toReplace = Util.ReadLine("captured replacement?",replaceDefault);
	var namedReplacement = toReplace.Contains("(?<")? @"\k<" + toReplace.After("(?<").Before(">") +">" : string.Empty;
	var isSingleReplace =toReplace.Contains("(?<") &&  output.IndexOf(toCapture) >=0 && output.IndexOf(toCapture,output.IndexOf(toCapture)) == -1;
	new{ isSingleReplace, toCapture,toReplace}.Dump();
	if(isSingleReplace)
		output = output.Replace(toCapture,toReplace).Dump();
	else
		output = (output.Before(toCapture) + toReplace + output.After(toCapture).Replace(toCapture,namedReplacement)).Dump();
	
}