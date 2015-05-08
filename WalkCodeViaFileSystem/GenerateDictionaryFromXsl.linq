<Query Kind="Statements" />

var text=System.IO.File.ReadAllText(@"C:\Projects\Service\App_Data\config.xsl");
var regex= Regex.Matches(text,"<xsl:variable name=\"(.*?)\" (?:select=\"(.*?)\")?");
var sb=new StringBuilder();
foreach(var m in regex.Cast<Match>())
{
	sb.AppendLine("{"+m.Groups[1].Captures[0].Value+(m.Groups[2].Captures.Count>0?",\""+ m.Groups[2].Captures[0].Value+"\"":"null")+"},");
}
sb.ToString().Dump();