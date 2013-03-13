<Query Kind="Program" />

///Find aspx code tags in a .aspx or .ascx file
///Find any of the same in the related .js file
///dump transformed .js code to output
///Assumes that you copy/pasted the raw js script element contents from aspx into js.
void Main()
{
//d:\projects\psa\GTPM\Web\PSAT.WebApp\Pages\ClientProfilewithDeals.aspx
var basePath=@"d:\projects\psa\gtpm\web\psat.webapp\";

var htmPath=basePath+@"Pages\ClientProfilewithDeals.aspx";
var jsPath=basePath+@"scripts\page\ClientProfilewithDeals.js";

if(System.IO.File.Exists(htmPath)==false)
{ "Htm File not found".Dump(); return;}

if(System.IO.File.Exists(jsPath)==false)
{ "Js File not found".Dump(); return;}
	//var patterns=new[]{"*.aspx","*.js"};

var jsBad=GetMatches(jsPath,@"<%.*%>",m=>m.Value);//search for asp tags in js
if(jsBad.Any())
	LINQPad.Util.Highlight( jsBad).Dump("Asp tags in js");
var htms=GetMatches( //search for <%= *.ClientID %>
	htmPath,@"<%=\s*(\w+\.ClientID)\s*%>", m=>m.Groups[1].Value);
	
	htms.Dump("htm");
	
	//search for any .ClientID calls in the javascript
var jsms=GetMatches(jsPath,@"(\w*\.ClientID)",m=>m.Groups[1].Value);
	jsms.Dump("js");	
var newJavascript=string.Empty;
foreach(var item in jsms)
	newJavascript+="var "+item.StringBeforeOrSelf(".ClientID")+"= { ClientID: \"<%="+item.StringBeforeOrSelf(".ClientID")+".ClientID%>\" };"+Environment.NewLine;
newJavascript.Dump();
//search for <script> </script>

} //end main

IEnumerable<Match> GetMatches(string text, string pattern)
{
var regex=new Regex(pattern,RegexOptions.IgnoreCase | RegexOptions.Compiled);
	foreach(var match in regex.Matches(text).Cast<Match>())
	yield return match;
}
IEnumerable<string> GetMatches(string path, string pattern,Func<Match,string> selector)
{
	var items=new HashSet<string>();
		var text= System.IO.File.ReadAllText(path);
		foreach(var match in GetMatches(text,pattern))
		{
			items.Add(selector(match));
		}
return items.OrderBy(f=>f);
}

public static class StringExtensions
{
public static string StringBeforeOrSelf(this string text, string delimiter)
	{
	if(text.Contains(delimiter)==false)
	return text;
	return text.StringBefore(delimiter);
	}
	public static string StringBefore(this string text, string delimiter)
	{
	return text.Substring(0,text.IndexOf(delimiter));
	}
public static string StringAfterOrSelf(this string text, string delimiter)
	{
	if(text.Contains(delimiter)==false)
	return text;
	return text.StringAfter(delimiter);
	}
	public static string StringAfter(this string text, string delimiter)
	{
		return text.Substring( text.IndexOf(delimiter)+delimiter.Length);
	}
	/// <summary>
	/// Join a list of strings with a separator
	/// From BReusable
	/// </summary>
	/// <param name="l"></param>
	/// <param name="seperator"></param>
	/// <returns></returns>
	public static String DelimitLarge(this IEnumerable<string> l, string separator)
	{
		var counter = 0;
	
		var result = new StringBuilder();
		foreach (var item in l)
		{
			if (counter != 0) result.Append(separator);
			result.Append(item);
			counter++;
		}
		return result.ToString();
	}

}