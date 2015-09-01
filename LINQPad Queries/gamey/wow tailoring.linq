<Query Kind="Statements">
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

// purpose, determine how many of what are required to make bags
// without hardcoding the bag types and components
// i.e. someone asks for a silk bag, this returns how many silk cloth would be needed (optionally any thing else required to make the bag)


// Util.Cache tells linqpad to pull this variable in once, and reuse the variable on every execution, instead of everytime the script runs
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
	
	
	.Select(x => 
		new{ Mapped= 
			Newtonsoft.Json.JsonConvert.DeserializeAnonymousType(x,new{
				template="",id="",name="",tabs="",
					data=new []{ 
						new {cat=1,creates=new int []{},id=1,learnedat=1,name="",reagents = new int[][]{}
					}	
				}, 
				}),
		x})
	//.Dump("rRow")
		//throw away raw view, select just the mapped data
	.SelectMany(x =>
		x.Mapped.data)
	.Where(x => x.name.EndsWith("Bag") || x.name.Contains("Bolt"))	
	.Select(x => new {x.cat,Creates =x.creates !=null? x.creates[0]:0, x.id,x.learnedat, Name=x.name.Substring(1),Reagents = x.reagents.Select(r =>new KeyValuePair<int,int>(r[0],r[1]))})
	.OrderByDescending(x => x.Name.EndsWith("Bag"))
	.ThenByDescending(x=>x.learnedat)
	//.Dump("recipe rows")
	;
	
	
var bagRecipes = from r in recipeRows
	let reagents = 
		from reag in r.Reagents 
		join itemL in itemRows
		on reag.Key equals itemL.id into itemLeft
		from itemRow in itemLeft.DefaultIfEmpty()
		select new{
			Id=reag.Key,
			Count = reag.Value,
			Name=itemRow != null? itemRow.Mapped.name_enus : null
		
		//,Total = total
		}
	
	select new{
		r.id,
		r.Name,
		Requirements =reagents
		//, r.Reagents
	};
	
	bagRecipes.Dump();

var bagRecipesDisplay = 
	from br in bagRecipes
	let cloths = br.Requirements.FirstOrDefault(brr => brr.Name != null)
	select new{ Name=new Hyperlinq("http://www.wowhead.com/spell="+br.id+"&power", br.Name), br.id, br.Requirements, cloths};
	
	bagRecipesDisplay.Dump();
//	
//		//let parts = recipeRows.Where(p => p.id== 
//		let subcomponents = // things that can be created with tailoring
//			//recipeRows.Where(boltRecipes => boltRecipes.id == i.id).Select(br => br.Reagents).First(): Enumerable.Empty<KeyValuePair<int,int>>()
//			recipeRows
//			.Where(br => br.Creates == reag.Key)
//			.Dump("recipe row")
//			.Select(rr => 
//				rr.Reagents.Select(subR =>
//					new{Count= subR.Value,Item=itemRows.Select(subI => new{subI.id,Name= subI.Mapped.name_enus }).First(subI => subI.id == subR.Key)}
//					)
//			) //.FirstOrDefault()
//		let cloths = subcomponents.FirstOrDefault(sc => sc.FirstOrDefault(sc1 => sc1.Item.Name.EndsWith("Cloth")) != null)
//		//let reagentKeys = reagents.Select(reag => reag.id).ToArray()
//		let cloth = cloths !=null? cloths.Select(c => new {c.Count, c.Item.Name}).FirstOrDefault() : null
//		//let total = 