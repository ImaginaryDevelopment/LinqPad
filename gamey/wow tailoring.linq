<Query Kind="Statements">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

var wiki= Util.Cache( () => {
	var src ="http://wowwiki.wikia.com/Tailoring_patterns";
	string html;
	using (var client =new System.Net.Http.HttpClient())
	{
		html = client.GetStringAsync(src).Result;
	
	}
	var doc = new HtmlAgilityPack.HtmlDocument();

	doc .LoadHtml(html);
	html = null;
	GC.Collect();
	//var nav =doc.CreateNavigator();
	// http://stackoverflow.com/questions/1604471/how-can-i-find-an-element-by-css-class-with-xpath
	return doc.DocumentNode.SelectNodes("//table[contains(concat(' ',@class,' '),' darktable ')]//tr").ToArray();
	//return nav;
});

//var rows = wiki
//	.Select("")
//	.Cast<XPathNavigator>();
var bagRows= wiki
	.Where(x => x.InnerHtml.Contains("Bag"))
	.Where(x => x.InnerHtml.Contains("Bags") == false)
	.Select(x => x.InnerHtml.ToString())
	.Dump();
var boltRows = wiki
	.Where(x => x.InnerHtml.Contains("title=\"Bolt of "))
	.Where(x => x.InnerHtml.Contains("Bag") == false)
	.Select(x => new {
		Children=x.ChildNodes.Where(cn => String.IsNullOrWhiteSpace(cn.OuterHtml) ==false).Select(cn => cn.OuterHtml) 
		})
	.Dump("bolts");
//var bags = bagRows.Select(r=> new HtmlAgilityPack.HtmlEntity(){  })