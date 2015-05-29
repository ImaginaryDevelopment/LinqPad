<Query Kind="Statements">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

var wiki= Util.Cache( () => {
	var src ="http://www.wowhead.com/skill=197/tailoring";
	//item lookup? http://www.wowhead.com/item=2589&power
	string html;
	using (var client =new System.Net.Http.HttpClient())
	{
		html = client.GetStringAsync(src).Result;
		//html.Dump();
	}
	var doc = new HtmlAgilityPack.HtmlDocument();

	doc .LoadHtml(html);
	html = null;
	GC.Collect();
	//var nav =doc.CreateNavigator();
	// http://stackoverflow.com/questions/1604471/how-can-i-find-an-element-by-css-class-with-xpath
	return doc.DocumentNode.SelectNodes("//script").ToArray();//div[id='tab-recipes']//table//tr").ToArray();
	//return nav;
});

var textPairRegex = new Regex("\"name_enus\":\"[^\"]+\"");
//var rows = wiki
//	.Select("")
//	.Cast<XPathNavigator>();
var itemRows= wiki
	//.Where(x => x.InnerHtml.Contains("name: LANG.tab_recipes"))
	//.Take(4)
	//.Dump("recipes")
	.SelectMany(x => Regex.Matches(x.InnerText,@"_\[(\d+)\]\s?=\s?({[^;]+});").Cast<Match>().Select(m => new{id=int.Parse(m.Groups[1].Value),Json= m.Groups[2].Value}))
	.Select(x => 
		new { x.id,
		Mapped=
			Newtonsoft.Json.JsonConvert.DeserializeAnonymousType(x.Json,new{name_enus="", quality=1, icon="",screenshot=1, attainable=0, jsonEquip=new {avgBuyout=new Nullable<int>(),BuyPrice=new Nullable<int>(),SellPrice=1,reqskill=new Nullable<int>()}})
			
		,x}
			
		)
	.Where(x => x.Mapped.name_enus.Contains("bag", StringComparison.InvariantCultureIgnoreCase) || x.Mapped.name_enus.Contains(" cloth", StringComparison.InvariantCultureIgnoreCase))
	
	//.Where(x => x.InnerHtml.Contains("Bags") == false)
	//.Select(x => x.InnerHtml.ToString())
	//.Dump()
	;
	
var recipeRows = wiki
	.Where(x => x.InnerHtml.Contains("new Listview({template: 'spell', id: 'recipes',"))
	.Select(x => x.InnerHtml.After("new Listview("))
	.Select(x => x.Before("new Listview(").BeforeLast("}") +"}")
	.Select(x => x.Replace("name: LANG.tab_recipes, tabs: tabsRelated,",string.Empty))
	.Select(x => Regex.Replace(x,@"note: \$WH.sprintf\([^)]+\),",String.Empty))
	//.Dump()
	
	.Select(x => 
		new{ Mapped= 
			Newtonsoft.Json.JsonConvert.DeserializeAnonymousType(x,new{
				template="",id="",name="",tabs="",
					data=new []{ 
						new {cat=1,creates=new int []{},id=1,learnedat=1,name="",reagents = new int[][]{}
					}	
				}, 
				//new Newtonsoft.Json.JsonSerializerSettings(){ TypeNameHandling= Newtonsoft.Json.TypeNameHandling.None, PreserveReferencesHandling= Newtonsoft.Json.PreserveReferencesHandling.None, MissingMemberHandling= Newtonsoft.Json.MissingMemberHandling.Ignore 
				}),
		x})
	.SelectMany(x =>
		x.Mapped.data)
	.Where(x => x.name.EndsWith("Bag") || x.name.Contains("Bolt"))	
	.Select(x => new {x.cat,Creates = x.creates[0], x.id,x.learnedat, Name=x.name.Substring(1),Reagents = x.reagents.Select(r =>new KeyValuePair<int,int>(r[0],r[1]))})
	
	.Dump();
var q = from r in recipeRows
	let reagents = 
		from reag in r.Reagents 
		join i in itemRows 
		on reag.Key equals i.id 
		//let //clothForBolts = //i.Mapped.name_enus.Contains("Bolt") ? 
		let subcomponents = 
			//recipeRows.Where(boltRecipes => boltRecipes.id == i.id).Select(br => br.Reagents).First(): Enumerable.Empty<KeyValuePair<int,int>>()
			recipeRows.Where(br => br.Creates == reag.Key).Select(rr => new { reag.Key, rr.Name,rr.Reagents}).FirstOrDefault()
		let total = subcomponents!=null && subcomponents.Reagents.Any()? (subcomponents.Reagents.Sum(subReag =>subReag.Value * reag.Value )) : 0
		select new{i.id,reag.Value,i.Mapped.name_enus,subcomponents
		,Total = total
		}
	
	select new{r.id, r.Name,reagents};
	
	q.Dump();